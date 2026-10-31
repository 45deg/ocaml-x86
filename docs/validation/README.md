# 起動検証ログ

開発時に取得したゲストのCOM1出力とホストの進捗記録。
これらは記録時の実装・入力に対する証跡であり、現在のチェックアウトで
Linux起動を再実行した結果ではない。再現手順と入力の識別値は
[Alpine](../../fixtures/alpine/README.md)と[Debian](../../fixtures/debian/README.md)を参照。

## シェル起動・コマンド実行の成功ログ

| ログ | 入力・条件 | 確認結果 |
|---|---|---|
| `alpine-bzimage-shell.log` | Alpine 3.22.7 x86、bzImage、gzip initramfs、128 MiB | BusyBox ash、計算、外部コマンド、バージョン表示 |
| `debian-slim-serial.log` | Debian 6.1.0-50-686、ELF、非圧縮cpio、512 MiB | dash、計算、uname、Debian 12.15、proc/sysfs |
| `debian-bzimage-serial.log` | 同Debian、bzImage、非圧縮cpio、512 MiB | 同上 |
| `debian-bzimage-gzip-serial.log` | 同Debian、bzImage、gzip cpio、512 MiB | 同上 |

Debianの成功ログは2026-11-01の記録。上の3条件では暗号自己テストを省略していない。
確認入力の完了は、順に約45〜46億、69億、85億予算ステップまで。
記録後にホストから終了させた。対応する`-state.log`は進捗記録であり、
SIGINTによる終了のため最終レジスタダンプは含まない。

リポジトリのルートで保存済みログを検証できる。

```sh
python3 scripts/check_shell_log.py docs/validation/alpine-bzimage-shell.log
python3 scripts/check_debian_shell_log.py --slim docs/validation/debian-slim-serial.log
python3 scripts/check_debian_shell_log.py --slim docs/validation/debian-bzimage-serial.log
python3 scripts/check_debian_shell_log.py --slim docs/validation/debian-bzimage-gzip-serial.log
```

入力のエコーではなく、ゲストが返した結果を確認する。
端末のjob controlは無効で、TSC/PITのずれも検出される。
これらの成功はx86全機能の互換性やディスク・ネットワークの動作を示さない。

## 過去の切り分け記録

次は当時の途中経過であり、現在の到達点は上記の成功ログを参照。

| ログ | 記録時の結果 |
|---|---|
| `alpine-elf-serial.log` | initramfsなしのELF起動。IRQ・console・タイマー初期化後、FPU不在の診断 |
| `alpine-rootfs-serial.log`、`alpine-rootfs-state.log` | x87追加後にinitramfs展開を通過し、未実装だったVERWで停止 |
| `alpine-shell-prompt.log` | ELF経路でashのプロンプト到達。入力トリガー不一致で確認コマンドは未送信 |
| `debian-cmpxchg8b-serial.log`、`debian-cmpxchg8b-state.log` | CMPXCHG8B修正後、installer initramfsを使用。RCU初期化後に命令予算上限 |
| `debian-slim-notests-serial.log`、`debian-slim-notests-state.log` | ELF・非圧縮cpioでシェル確認成功。`cryptomgr.notests=1`で暗号自己テストを省略 |
