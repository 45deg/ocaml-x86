# ocaml-x86

OCamlで実装した32-bit x86インタプリタ。ハードウェア仮想化や既存エミュレータを使わず、Linuxカーネルとユーザーランド（Alpine / Debian）を起動し、シリアル端末からシェルを操作する。

## コード規模

実装本体（`lib/*.ml`・`bin/*.ml`）の行数。空行・テスト・補助スクリプトは除外。

| コメントあり | コメントなし |
|---:|---:|
| 4,079行 | 3,782行 |

## ビルドと実行

必要環境：Unix系ホスト（64-bit推奨）、opam、OCaml 4.14以上、Dune 3.17以上、Base v0.16以上、Zarith、Python 3.9以上（補助スクリプト用）。

```sh
opam install . --deps-only
dune build
```

raw binaryを実行する場合（16 MiB RAMの`0x1000`へロード、32-bit flat状態）：

```sh
dune exec ocaml-x86 -- --steps 1000000 path/to/program.bin
```

全オプションは`dune exec ocaml-x86 -- --help`で確認できる。32-bit環境でのRAM上限は1023 MiB、入力ファイル（カーネル・initramfs等）の各サイズ上限は約16 MiB未満。

## Linuxの起動

カーネルとrootfsを取得し、`make_initramfs.py`でinitramfsを作成して起動する（要`curl`）。

### Alpine Linux (3.22.7 x86)

```sh
mkdir -p fixtures/alpine
curl -fL https://dl-cdn.alpinelinux.org/alpine/v3.22/releases/x86/netboot-3.22.7/vmlinuz-lts \
  -o fixtures/alpine/vmlinuz-lts
curl -fL https://dl-cdn.alpinelinux.org/alpine/v3.22/releases/x86/alpine-minirootfs-3.22.7-x86.tar.gz \
  -o fixtures/alpine/alpine-minirootfs-3.22.7-x86.tar.gz
python3 scripts/make_initramfs.py fixtures/alpine/alpine-minirootfs-3.22.7-x86.tar.gz \
  fixtures/alpine/initramfs.cpio.gz
./scripts/run_alpine.sh
```

### Debian 12 (bookworm i386 slim)

```sh
mkdir -p fixtures/debian
curl -fL https://deb.debian.org/debian/dists/bookworm/main/installer-i386/current/images/netboot/debian-installer/i386/linux \
  -o fixtures/debian/linux
curl -fL https://raw.githubusercontent.com/debuerreotype/docker-debian-artifacts/dist-i386/bookworm/slim/oci/blobs/rootfs.tar.gz \
  -o fixtures/debian/bookworm-i386-slim-rootfs.tar.gz
python3 scripts/make_initramfs.py fixtures/debian/bookworm-i386-slim-rootfs.tar.gz \
  fixtures/debian/slim-initramfs.cpio.gz
./scripts/run_debian.sh
```

### ゲスト内でのセルフコンパイル

事前ビルド済みのツールチェーン（OCaml 4.14.2、GNU binutils 2.38、依存ライブラリ）を含むrootfsとAlpineカーネルを使い、32-bit Linuxゲスト内でこのエミュレータ自身をビルド・実行できる。

```sh
./scripts/run_self_compile.sh              # ネイティブ版 (ocamlopt + GNU as/ld)
./scripts/run_self_compile.sh --ocamlrun   # バイトコード版 (ocamlrun + ocamlc.byte)
```

- 既定で`artifacts/selfcompile-native/rootfs.tar.gz`（ネイティブ版）または`artifacts/selfcompile/rootfs.tar.gz`（バイトコード版）を使用する（`SELF_COMPILE_ROOTFS`で変更可）。
- 現在のソースを注入したinitramfsを`artifacts/selfcompile-<mode>-run/`に生成し、ゲスト内でコンパイル・リンク後、生成されたバイナリでraw x86プログラムを実行する（`--prepare-only`でイメージ生成のみ実行可）。

### 補足

- `make_initramfs.py`はrootfsアーカイブをホストに展開せず、デバイスノードと`/init`（`/proc`・`/sys`をマウントして`/bin/sh -i`を起動）を追加したnewc initramfsへ変換する。任意のrootfs tarball（要`/bin/sh`、`mount`）にも使用できる。
- 各起動スクリプトには`--steps N`などのCLIオプションを渡して既定値を上書きできる。終了はホスト側でCtrl-Cを押す。
- `--serial-input FILE --input-after TEXT`で、指定文字列の出力後に一度だけ自動入力できる。
- 開発時は`extract_kernel.py`でbzImageからELFを抽出し、`--kernel-elf`でgzip展開を省略できる（起動情報の取得に`--kernel`も必要）：

```sh
python3 scripts/extract_kernel.py path/to/bzImage path/to/vmlinux
dune exec ocaml-x86 -- --kernel path/to/bzImage \
  --kernel-elf path/to/vmlinux --steps 200000000 --dump-state
```

## 実装範囲

- **CPU（整数）**：8/16/32-bit演算、ModR/M・SIB、16-bitアドレス指定、乗除算、シフト・回転、ビット操作、条件転送、スタック、文字列、LOCK検査。
- **保護モード**：GDT/LDT、セグメントキャッシュ、アクセス検査、CR0/2/3/4、IDT、TSSの特権スタック、INT/IRET、例外・外部割り込み、STI/MOV SSの割り込み抑制。
- **MMU**：legacy（4 KiB/4 MiB）およびPAE（4 KiB/2 MiB）ページング、権限検査、CR0.WP、A/Dビット、#PF、TLBキャッシュ（ページテーブル書き込み・CR0/3/4変更・INVLPGで無効化）。仮想RAMは`Bigarray`で保持。
- **x87 FPU**：状態管理（FNINIT/FNCLEX/FWAIT、FLDCW/FNSTCW/FNSTSW、環境保存・復元、FNSAVE/FRSTOR）、データ転送（FLD/FST、FILD/FIST、FXCH）、NaN・±∞・subnormalのロード/ストア、有限値の四則演算・比較・FPREM。Zarithの有理数で中間計算し、PC=24/53/64と丸め方向を適用。
- **周辺機器**：8259 PIC、PIT、COM1 UART、CMOS時計、空のPCIバス。ポートI/O（8/16/32-bit）、HLTからのIRQ復帰。時刻は仮想100 MHzで進行。
- **ローダー**：Boot Protocol 2.10以降の32-bit bzImageと開発用ELF32（boot_params、E820、GDT、initramfs配置）。RAM既定値は128 MiB（`--memory-mib`で変更可）。
- **未対応**：リアルモード、VM86、16-bitスタック/IRET、ハードウェアタスク切替、MMX/SSE、x87の非有限値演算・超越関数・非マスク例外、BIOS/UEFI、APIC、ディスク、画面、ネットワーク。

### 命令対応表

[命令対応マップ（SVG）](docs/instructions.svg)

| 分類 | 対応 | 未対応・制約 |
|---|---|---|
| アドレス指定 | ModR/M、SIB、disp8/16/32、0x67（16-bit）、セグメントoverride | - |
| データ転送 | MOV、MOVSX/ZX、LEA、XCHG、XLAT、BSWAP | 16-bit BSWAP、far pointer load |
| 算術・論理 | ADD/ADC/SUB/SBB/CMP、AND/OR/XOR/TEST、INC/DEC/NEG/NOT | BCD命令（DAA/DAS/AAA/AAS/AAM/AAD） |
| 乗除算 | MUL/IMUL（1/2/3 operand）、DIV/IDIV | - |
| シフト・回転 | SHL/SAL/SHR/SAR、ROL/ROR/RCL/RCR、SHLD/SHRD | - |
| ビット | BT/BTS/BTR/BTC、BSF/BSR | - |
| 条件 | Jcc、SETcc、CMOVcc（全16条件） | - |
| スタック | PUSH/POP、PUSHF/POPF、PUSHA/POPA、ENTER/LEAVE | 16-bitスタックアドレス |
| 制御移行 | near CALL/JMP/RET、直接far JMP、LOOP系、JCXZ/JECXZ、INT/32-bit IRET、NOP（複数バイト含む） | far CALL/RET、16-bit IRET |
| 文字列 | MOVS/STOS/LODS/CMPS/SCAS、REP/REPE/REPNE | INS/OUTS |
| フラグ | CLC/STC/CMC、CLD/STD、LAHF/SAHF、CLI/STI | - |
| アトミック | CMPXCHG、CMPXCHG8B、XADD、XCHG、LOCK prefix | 単一CPUのみ |
| システム | GDT/IDT/LDT/TSS、CR、paging、ring遷移、CPUID、限定MSR | タスク切替、VM86、debugレジスタ |
| 浮動小数点・SIMD | x87基本命令・FPREM（有限値）、NaN・±∞・subnormalのロード/ストア | x87超越関数・非有限値演算・非マスク例外、FXSAVE、MMX、SSE |

## コード規約

- ライブラリは`Base`を使用し、ホストI/Oは`bin/`に分離する。
- 主型は`t`、識別子は`snake_case`とし、型・関数定義とAPIドキュメントは`.ml`に集約する。
- 固定幅演算は`Stdlib.Int32`、符号なし拡張は`Stdlib.Int64`を明示する。
- ゲストの失敗は構造化して扱い、ホスト例外を包括的に握り潰さない。
- フォーマットはOCamlFormat（`conventional`プロファイル）に従う。
