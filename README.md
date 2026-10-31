# ocaml-os

OCamlで実装した32-bit x86インタプリタ。ハードウェア仮想化や既存エミュレータを使わず、Linuxカーネルとユーザーランド（Alpine / Debian）を起動し、シリアル端末からシェルを操作する。

## ビルドと実行

必要環境：64-bit Unix系ホスト、opam、OCaml 4.14以上、Dune 3.17以上、Base v0.16以上、Zarith、Python 3.9以上（補助スクリプト用）。

OCaml 4.14ではBase v0.16系を使用する。仮想RAMはBigarrayに保持し、単一の`bytes`のサイズ制限を回避している。32-bitホストでの動作は未検証で、RAMの指定上限は1023 MiBになる。また、入力ファイルは`string`へ一括で読み込むため、32-bit環境ではカーネル・initramfsなど各ファイルのサイズが約16 MiB未満である必要がある。

```sh
opam install . --deps-only
dune build
```

raw binaryを実行する場合（16 MiB RAMの`0x1000`へロード、32-bit flat状態）：

```sh
dune exec ocaml-os -- --steps 1000000 path/to/program.bin
```

全オプションは`dune exec ocaml-os -- --help`で確認できる。

## Linuxの起動

カーネルとrootfsを取得し、`make_initramfs.py`でinitramfsを作成して起動する（要`curl`、`shasum`または`sha256sum`）。

### Alpine Linux (3.22.7 x86)

```sh
mkdir -p fixtures/alpine
curl -fL https://dl-cdn.alpinelinux.org/alpine/v3.22/releases/x86/netboot-3.22.7/vmlinuz-lts \
  -o fixtures/alpine/vmlinuz-lts
curl -fL https://dl-cdn.alpinelinux.org/alpine/v3.22/releases/x86/alpine-minirootfs-3.22.7-x86.tar.gz \
  -o fixtures/alpine/alpine-minirootfs-3.22.7-x86.tar.gz
cd fixtures/alpine
printf '%s  %s\n' \
  90b1bf42ca7d1ff28c6a32962747a1e3083294629607ce62e7a1734de04af13e vmlinuz-lts \
  cb3459c743d0f2e0236c79a0abc49956b6d1190a709da51aa1c2acbc9e1e0c66 alpine-minirootfs-3.22.7-x86.tar.gz \
  | shasum -a 256 -c
cd ../..
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

検証時のSHA256（配布URLは更新される場合あり）：
- `linux` (6.1.0-50-686): `c77cd9c37609b8b1b1cdfbb91b644a44d606d3d75da8656a0c79635be0b89c21`
- `bookworm-i386-slim-rootfs.tar.gz`: `8268d978265dc07d9054d3d23c581ec1e6c2d82b134331d09069a923d04dc48a`

### 補足

- `make_initramfs.py`はrootfsアーカイブをホストに展開せず、デバイスノードと`/init`（`/proc`・`/sys`をマウントして`/bin/sh -i`を起動）を追加したnewc initramfsへ変換する。任意のrootfs tarball（要`/bin/sh`、`mount`）にも使用できる。
- `run_alpine.sh` / `run_debian.sh`には`--steps 2000000000`などのCLIオプションを渡して既定値を上書きできる。終了はホスト側でCtrl-Cを押す。
- `--serial-input FILE --input-after TEXT`で、指定文字列の出力後に一度だけ自動入力できる。
- 開発時は`extract_kernel.py`でbzImageからELFを抽出し、`--kernel-elf`でgzip展開を省略できる（起動情報の取得に`--kernel`も必要）：

```sh
python3 scripts/extract_kernel.py path/to/bzImage path/to/vmlinux
dune exec ocaml-os -- --kernel path/to/bzImage \
  --kernel-elf path/to/vmlinux --steps 200000000 --dump-state
```

## 実装範囲

- **CPU（整数）**：8/16/32-bit演算、ModR/M・SIB、16-bitアドレス指定、乗除算、シフト・回転、ビット操作、条件転送、スタック、文字列、LOCK検査。
- **保護モード**：GDT/LDT、セグメントキャッシュ、アクセス検査、CR0/2/3/4、IDT、TSSの特権スタック、INT/IRET、例外・外部割り込み、STI/MOV SSの割り込み抑制。
- **MMU**：legacy（4 KiB/4 MiB）およびPAE（4 KiB/2 MiB）ページング、権限検査、CR0.WP、A/Dビット、#PF、TLBキャッシュ（ページテーブル書き込み・CR0/3/4変更・INVLPGで無効化）。
- **x87 FPU**：状態管理（FNINIT/FNCLEX/FWAIT、FLDCW/FNSTCW/FNSTSW、環境保存・復元、FNSAVE/FRSTOR）、データ転送（FLD/FST、FILD/FIST、FXCH）、有限値の四則演算と比較。Zarithの有理数で中間計算し、PC=24/53/64と丸め方向を適用。
- **周辺機器**：8259 PIC、PIT、COM1 UART、CMOS時計、空のPCIバス。ポートI/O（8/16/32-bit）、HLTからのIRQ復帰。時刻は仮想100 MHzで進行。
- **ローダー**：Boot Protocol 2.10以降の32-bit bzImageと開発用ELF32（boot_params、E820、GDT、initramfs配置）。RAM既定値は128 MiB（`--memory-mib`で変更可）。
- **未対応**：リアルモード、VM86、16-bitスタック/IRET、ハードウェアタスク切替、MMX/SSE、x87の非有限値・超越関数・例外フラグ、BIOS/UEFI、APIC、ディスク、画面、ネットワーク。

### 命令対応表

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
| 制御移行 | near CALL/JMP/RET、直接far JMP、LOOP系、JCXZ/JECXZ、INT/32-bit IRET | far CALL/RET、16-bit IRET |
| 文字列 | MOVS/STOS/LODS/CMPS/SCAS、REP/REPE/REPNE | INS/OUTS |
| フラグ | CLC/STC/CMC、CLD/STD、LAHF/SAHF、CLI/STI | - |
| アトミック | CMPXCHG、CMPXCHG8B、XADD、XCHG、LOCK prefix | 単一CPUのみ |
| システム | GDT/IDT/LDT/TSS、CR、paging、ring遷移、CPUID、限定MSR | タスク切替、VM86、debugレジスタ |
| 浮動小数点・SIMD | x87基本命令（有限値） | x87超越関数・非有限値、FXSAVE、MMX、SSE |

## コード規約

- ライブラリは`Base`を使用し、ホストI/Oは`bin/`に分離する。
- 主型は`t`、識別子は`snake_case`とし、型・関数定義とAPIドキュメントは`.ml`に集約する。
- 固定幅演算は`Stdlib.Int32`、符号なし拡張は`Stdlib.Int64`を明示する。
- ゲストの失敗は構造化して扱い、ホスト例外を包括的に握り潰さない。
- フォーマットはOCamlFormat（`conventional`プロファイル）に従う。
