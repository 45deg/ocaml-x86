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

## x87部分実装

FNINIT/FNCLEX/FWAIT、FLDCW/FNSTCW/FNSTSW、環境保存・復元、FNSAVE/FRSTOR、
FLD/FST、FILD/FIST、FXCH、基本加減乗除と比較を追加した。
有限値の計算にはZarithの有理数を使い、PC=24/53/64と4種類の丸め方向を適用する。
80-bit保存領域のペイロードとTOPは保持する。非有限値・スタック例外・範囲外の
変換は現時点では明示的な未対応停止。例外フラグ、非正規数のIEEE格納、超越関数、
FXSAVE/SSEは未対応であり、x87全体の互換性を示す実装ではない。
