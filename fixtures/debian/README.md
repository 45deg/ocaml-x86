# Debian 12 i386 slim 起動検証

公式Debian 6.1.0-50-686のbzImageとBookworm i386 slim rootfsから作成した
initramfsを使用する。512 MiB RAM、通常の暗号自己テストを有効にした条件で、
`/init`、dashシェル、確認コマンドまで到達した。
カーネルELFの事前抽出とinitramfsの事前解凍は不要。

## 入力の準備

以下はリポジトリのルートで実行する。`curl`、`gzip`、Python 3.9以上、
SHA256検証用の`shasum`が必要。`shasum -a 256 -c`は`sha256sum -c`でも代用できる。
バイナリと生成物はGit管理対象外。

```sh
curl -fL https://deb.debian.org/debian/dists/bookworm/main/installer-i386/current/images/netboot/debian-installer/i386/linux \
  -o fixtures/debian/linux
curl -fL https://raw.githubusercontent.com/debuerreotype/docker-debian-artifacts/dist-i386/bookworm/slim/oci/blobs/rootfs.tar.gz \
  -o fixtures/debian/bookworm-i386-slim-rootfs.tar.gz
python3 scripts/extract_kernel.py fixtures/debian/linux fixtures/debian/vmlinux
python3 scripts/make_initramfs.py fixtures/debian/bookworm-i386-slim-rootfs.tar.gz \
  fixtures/debian/slim-initramfs.cpio --uncompressed
shasum -a 256 -c fixtures/debian/slim-SHA256SUMS
gzip -c -n fixtures/debian/slim-initramfs.cpio > fixtures/debian/slim-initramfs.cpio.gz
```

ELF抽出は`slim-SHA256SUMS`に記録した入力を照合するための手順で、通常起動では
`vmlinux`を使わない。両配布URLの内容は更新される。ハッシュが一致しなければ
保存済みログと同じ入力ではないため、バージョンと起動結果を改めて確認する。
記録したハッシュは入力の識別用であり、配布元の署名検証ではない。
`slim-gzip-SHA256SUMS`は検証時の圧縮ファイルの識別値で、gzip実装によって
圧縮結果が異なる場合がある。展開後のcpioは`slim-SHA256SUMS`で照合できる。

## シェル起動と検証

```sh
mkdir -p artifacts
dune build --profile release bin/main.exe
_build/default/bin/main.exe --kernel fixtures/debian/linux \
  --initrd fixtures/debian/slim-initramfs.cpio.gz --memory-mib 512 \
  --append 'console=ttyS0 earlyprintk=serial,ttyS0 nokaslr rdinit=/init' \
  --serial-input fixtures/debian/slim-shell-smoke.txt --input-after OCAML_OS_READY \
  --steps 18000000000 --progress-every 500000000 \
  > artifacts/debian-shell.log 2> artifacts/debian-state.log
python3 scripts/check_debian_shell_log.py --slim artifacts/debian-shell.log
```

保存済みのgzip起動ログでは約85億予算ステップまでに確認入力が完了した。
シェルはその後も入力を待つ。命令上限でのCLI終了コード1、または確認後に
Ctrl-Cで終了した場合の130は、ゲストコマンドの失敗を意味しない。
上の確認スクリプトでログ内の実行結果を検証する。`set -e`を使うスクリプトでは、
エミュレータの終了コードを別途処理する必要がある。
対話操作には`--serial-input`と`--input-after`の代わりに`--interactive`を指定する。

[保存済みログ](../../docs/validation/README.md)にはELF・非圧縮cpioによる起動と、
bzImage・非圧縮cpioによる起動も残している。`cryptomgr.notests=1`を使った
切り分け試験は、通常の暗号自己テスト有効の結果と区別している。

PID 1としてのシェル起動が検証対象で、端末のjob controlは無効。
systemdのサービス起動、インストール、ディスク・ネットワーク利用は未検証。
`SHA256SUMS`と`shell-smoke.txt`は初期のinstaller initramfs検証用に残した資料で、
上記のslim起動には使わない。
