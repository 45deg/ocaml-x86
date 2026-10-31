# ocaml-os

OCamlで実装する32-bit x86インタプリタ。目標はLinuxカーネルとAlpineの
ユーザーランドを起動し、シリアル端末からBusyBox shellを操作すること。
CPUの実行にハードウェア仮想化や既存エミュレータを使わない。

## ビルド

64-bitのUnix系ホストとopamが必要。OCaml 5.2以上、Dune 3.17以上、
Base v0.17以上、Zarithを使用する。検証済みの組み合わせは
OCaml 5.5.0、Dune 3.23.1、Base v0.17.3。補助スクリプトはPython 3.9以上を使う。
OCamlFormat 0.29.0のJane Streetプロファイルを使う。

```sh
opam install . --deps-only
dune build
```

実行にはraw binaryまたは`--kernel`が必要。引数なしでは使い方を表示し、
終了コード2で終了する。全オプションは`dune exec ocaml-os -- --help`で確認できる。
raw binaryは16 MiB RAMの`0x1000`へロードし、32-bit flat状態で実行する。

```sh
dune exec ocaml-os -- --steps 1000000 path/to/program.bin
```

## Linux直接起動

コマンドはリポジトリのルートで実行する。カーネルとrootfsは同梱しない。
32-bit x86用のbzImageと、必要に応じてinitramfsを用意する。

```sh
dune exec ocaml-os -- --kernel path/to/bzImage \
  --steps 1000000000 --dump-state
# initramfsを用意した場合:
dune exec ocaml-os -- --kernel path/to/bzImage \
  --initrd path/to/initramfs --steps 1000000000 \
  --append 'console=ttyS0 earlyprintk=serial,ttyS0 nokaslr rdinit=/bin/sh'
```

RAMの既定値は128 MiB（`--memory-mib`で変更可能）。Boot Protocol 2.10以降の32-bit bzImageを検証し、
boot_params、E820、GDT、引数、任意のinitramfsを配置する。
展開領域とinitramfsの重複を拒否する。BIOS/UEFIは実装しない。

開発時はgzip展開を省略し、同じカーネルから抽出した未改変ELFを実行できる。
`--kernel-elf`で指定したELFを使う場合も、起動情報の取得にbzImageが必要。

```sh
python3 scripts/extract_kernel.py path/to/bzImage path/to/vmlinux
dune exec ocaml-os -- --kernel path/to/bzImage \
  --kernel-elf path/to/vmlinux --steps 200000000 --dump-state
```

## 実装範囲

- 整数CPU：8/16/32-bit演算、ModR/M・SIB、16-bitアドレス指定、乗除算、
  シフト・回転、ビット操作、条件転送、スタック、文字列、LOCK検査。
- 保護モード：GDT/LDTとセグメントキャッシュ、アクセス検査、CR0/2/3/4、
  IDT、TSSの特権スタック、INT/IRET、例外・外部割り込み、STI/MOV SSの抑制。
- MMU：legacy 4 KiB/4 MiB、PAE 4 KiB/2 MiB、権限、CR0.WP、A/D、#PF。
  ページをまたぐ書き込みは先に全範囲を検査する。ページ変換をキャッシュし、
  ページテーブルへの書き込み、CR0/3/4変更、INVLPGで無効化する。
- PC：8259 PIC、PIT、COM1 UART、固定日付から進むCMOS時計、空のPCIバス。
  ポートIN/OUTのbyte/word/dword形式、HLTからIRQでの復帰。
  デバイス時刻はホスト時間から独立した仮想100 MHzで進める。
- ローダー：bzImageと開発用ELF32、配置検証、BSS初期化。

デバイスはLinux起動用の限定実装。PITの全モード、RTCの周期割り込み、
APIC、ディスク、画面、ネットワークは未対応。`--interactive`で標準入力をCOM1へ転送できる。
`--serial-input FILE --input-after TEXT`は指定文字列が出力されたら一度だけ入力する。16-bitスタックアドレス、16-bit IRET、リアルモード、
VM86、ハードウェアタスク切替、デバッグブレークポイント、MMX/SSEは未対応。
x87は状態管理、有限値の基本演算、整数・実数転送を部分実装。非有限値、
超越関数、例外フラグ・精度例外、IEEE変換の非正規数などは未対応。
有限値の中間計算はZarithの正確な有理数で行う。SIMD機能は広告しない。

命令形式の対応範囲は[命令カバレッジ](docs/instruction-coverage.md)を参照。

## コード規約

ライブラリはBaseを使い、ホストI/Oはbin/に置く。主型は`t`、名前は
snake_case。型・関数の定義とAPIの説明は.mlにまとめる。
固定幅演算はStdlib.Int32、符号なし拡張はStdlib.Int64を明示する。
ゲストの失敗を構造化して扱い、ホスト例外を包括的に握り潰さない。

参考：[OCaml Guidelines](https://ocaml.org/docs/guidelines)、
[Jane Street Base](https://github.com/janestreet/base)、
[Linux Boot Protocol](https://docs.kernel.org/arch/x86/boot.html)。

## rootfsからinitramfsを作成

Alpine minirootfsやDebian slimなどのrootfs tarballをnewc initramfsへ変換する。
アーカイブをホストへ展開せず、デバイスノードと`/init`を追加する。
rootfsには`/bin/sh`と`mount`が必要。`/init`はproc/sysfsをマウントし、
`/bin/sh -i`を起動する。

```sh
python3 scripts/make_initramfs.py path/to/rootfs.tar.gz path/to/initramfs.cpio.gz
dune exec ocaml-os -- --kernel path/to/bzImage \
  --initrd path/to/initramfs.cpio.gz --memory-mib 512 \
  --append 'console=ttyS0 earlyprintk=serial,ttyS0 nokaslr rdinit=/init' \
  --steps 18000000000 --interactive
```

シェルは入力を待ち続ける。実行上限に達するとCLIは終了コード1を返す。
端末のjob controlは無効。ディスク・ネットワークや通常のサービス起動には対応しない。
