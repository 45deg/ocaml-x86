# 整数CPUの対応範囲

既定32-bit operand/address size。表の「対応」は実装した形式を示し、
x86全体の完全な互換性やLinux起動成功を意味しない。

| 命令群 | 対応 | 残り・制約 |
|---|---|---|
| アドレス指定 | ModR/M、SIB、disp8/16/32、0x67による16-bit指定 | segment overrideとアクセス検査に対応 |
| データ転送 | MOV、MOVSX/ZX、LEA、XCHG、XLAT、BSWAP | 16-bit BSWAPは未定義なので拒否、遠隔ポインタload |
| 算術・論理 | ADD/ADC/SUB/SBB/CMP、AND/OR/XOR/TEST、INC/DEC/NEG/NOT、8/16/32-bit | BCD補正、AAM/AAD |
| 乗除算 | MUL/IMUL/DIV/IDIV、1/2/3 operand IMUL | #DEをIDTへ配送（Linuxモード） |
| シフト・回転 | SHL/SAL/SHR/SAR、ROL/ROR/RCL/RCR、SHLD/SHRD、1/imm8/CL | 未定義フラグは比較対象外 |
| ビット | BT/BTS/BTR/BTC、BSF/BSR、レジスタ・メモリ | 即値bit offsetはoperand内にmask |
| 条件転送 | 全16条件のJcc/SETcc/CMOVcc | メモリCMOVは条件不成立でも読み込み |
| スタック | PUSH/POP、PUSHF/POPF、PUSHA/POPA、ENTER/LEAVE | stack address sizeは32-bitのみ |
| 制御移行 | near CALL/JMP/RET、RET imm16、LOOP/LOOPE/LOOPNE、JCXZ/JECXZ | 直接far JMP、INT/32-bit IRET、例外配送にも対応。far CALL/RETは未対応 |
| 文字列 | MOVS/STOS/LODS/CMPS/SCAS、REP/REPE/REPNE、16/32-bit index/count | INS/OUTSは未対応。segment overrideは対応 |
| フラグ | CLC/STC/CMC、CLD/STD、LAHF/SAHF、CLI/STI | 特権チェックと割り込みshadowに対応 |
| 原子的更新 | CMPXCHG、CMPXCHG8B、XADD、XCHG | LOCK prefix対応。1 CPU |
| システム | GDT/IDT、segment/CR、paging、ring遷移、TSS/LDT、CPUID、限定MSR | 16-bit IRET、タスク切替、VM86、debug breakpoint |
| 拡張 | x87の状態管理・有限値基本演算（下記） | MMX、SSE/SSE2、x87の残り |

## 検証

`dune runtest`で境界値・メモリfault・命令シーケンスを検証する。
8-bit加減算262,144ケース、8-bit符号付き/符号なし乗算131,072ケースを含む。

任意の差分検証にはテスト専用Unicorn 2.1.4を使う。製品のライブラリ依存には含めず、
CPU実装の代用にはしない。テストのシードは固定し、定義されたフラグのみ比較する。

```sh
uv venv .venv
uv pip install --python .venv/bin/python unicorn==2.1.4
dune build test/instruction_probe.exe
.venv/bin/python scripts/differential.py
```

現在の26,000ケースは算術、シフト、乗除算、ビット、CMOV、CMPXCHG/XADD、
スタック、ENTER、SIB、16-bit addressing、即値形式を比較する。
レジスタ・EIP・定義されたEFLAGSと、指定した512バイトのメモリ領域を照合する。
全メモリ・全命令形式の網羅やハードウェア実機での確認ではない。

## 実カーネルの進捗

通常のbzImage経路でgzip展開後のstartup_32を通過し、ページングを有効化して
仮想アドレス上のカーネル初期化へ到達した。初期ページテーブル作成時の
RAM外読み込みはPC物理バスの未接続領域として扱う。仮想アドレスをRAMへ
無条件に別名対応させてはいない。

通常のbzImage経路でAlpineとDebian slimのシェル起動・コマンド実行を確認済み。
入力準備と再現手順は[README](../README.md)、証跡は[検証ログ](validation/README.md)を参照。

システム機能には次の回帰テストがある。

- legacy/PAE/PSE、WP・U/S・R/W、A/D、ページ境界とCR2。
- #PFハンドラがPTEを修正し、IRET後に失敗命令を再実行する機械語。
- TSSを使うring 3→0→3の往復、IDT権限エラー時の状態保存。
- STI shadow、非ゼロFSベース、PICのEOI・IRQ抑制、PIT、UART受信、PCI空バス。
- ELFの物理配置、仮想entry変換、BSS、壊れたセグメントの拒否。

整数26,000ケースの独立実装との差分テストはシステム機能追加後も通過。
以前のフラット起動区間では、開始から10,000,000ステップと続く1,000,000
ステップについて、汎用レジスタ・EIP・EFLAGSと128 MiB RAMの一致を確認した。
Unicorn側はREP終端の数え方で最初の区間が10,000,002ステップになる。
これはページング以降のシステム全体の差分検証や、実機での確認ではない。

比較の再現例（スナップショットは各128 MiB）:

```sh
mkdir -p artifacts
dune build test/kernel_probe.exe
_build/default/test/kernel_probe.exe fixtures/alpine/vmlinuz-lts 0 artifacts/boot0
_build/default/test/kernel_probe.exe fixtures/alpine/vmlinuz-lts 10000000 artifacts/boot10m
.venv/bin/python scripts/kernel_compare.py artifacts/boot0 --initial --steps 10000002 --compare artifacts/boot10m
```

開発用ELF起動ではシリアルログでIRQ初期化、ttyS0 console有効化、
PIT/TSC校正まで確認した。この時点ではFPU不在で停止していた。後述のx87実装でこの停止を通過した。
この到達点はELF経路での結果であり、通常bzImage経路の全工程を再検証した結果ではない。

## x87部分実装

FNINIT/FNCLEX/FWAIT、FLDCW/FNSTCW/FNSTSW、環境保存・復元、FNSAVE/FRSTOR、
FLD/FST、FILD/FIST、FXCH、基本加減乗除と比較を追加した。
有限値の計算にはZarithの有理数を使い、PC=24/53/64と4種類の丸め方向を適用する。
80-bit保存領域のペイロードとTOPは保持する。非有限値・スタック例外・範囲外の
変換は現時点では明示的な未対応停止。例外フラグ、非正規数のIEEE格納、超越関数、
FXSAVE/SSEは未対応であり、x87全体の互換性を示す実装ではない。

```sh
dune build test/fpu_probe.exe
.venv/bin/python scripts/fpu_compare.py
```

Unicornとの2,400ケース比較は有限値の80-bit演算結果を検証し、
追加の240ケースで比較命令のフラグとpopの効果を検証する。
例外・全フラグ・全命令形式の比較ではない。カーネルはFPUを認識してFSAVEを選択し、
従来のFPU不在による停止を通過した。rootfs付き起動の結果は[検証ログ](validation/README.md)を参照。

## Debian 12 / CMPXCHG8B

LOCK CMPXCHG8Bを追加し、CPUID.CX8を有効化した。成功・比較不一致、
operand-size prefix、非整列、#UD、RAM境界と書き込み不可ページを検証。
比較不一致時にも8バイト全体の書き込み権限を検査する。単一CPU実行中に
割り込みが命令途中へ入らない構成であり、SMPの検証ではない。

Debian 6.1.0-50-686の未改変ELFとinitramfsで50,000,000ステップ実行し、
従来のatomic64_read_cx8でのpanicを通過、RCU初期化まで到達。
この初期検証の終了理由は命令予算の上限。その後の通常bzImage経路での
Debian slimシェル起動結果は[検証ログ](validation/README.md)を参照。
記録はvalidation/debian-cmpxchg8b-serial.logと同-state.log。
