#!/usr/bin/env python3
"""Execute a saved flat-mode CPU/RAM snapshot with a test-only Unicorn oracle.
Kernel_probe captures GPRs/EIP/EFLAGS and RAM, not arbitrary CPU/device state.
--initial configures the project's Linux entry GDT. Without it, use only flat
code intervals that do not reload segment/control registers or access devices.
"""
import argparse
from pathlib import Path
from differential import REGISTERS
from unicorn import Uc, UcError, UC_ARCH_X86, UC_MODE_32
from unicorn.x86_const import (
    UC_X86_REG_EFLAGS, UC_X86_REG_EIP, UC_X86_REG_GDTR, UC_X86_REG_CR0,
    UC_X86_REG_CS, UC_X86_REG_DS, UC_X86_REG_ES, UC_X86_REG_SS,
    UC_X86_REG_FS, UC_X86_REG_GS,
)

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('snapshot', help='Prefix produced by kernel_probe')
parser.add_argument('--steps', required=True, type=int)
parser.add_argument('--initial', action='store_true')
parser.add_argument('--compare', help='Expected snapshot prefix')
args = parser.parse_args()
if args.steps <= 0:
    parser.error('--steps must be positive')
values = [int(x, 16) for x in Path(args.snapshot + '.regs').read_text().split()]
ram = Path(args.snapshot + '.ram').read_bytes()
mu = Uc(UC_ARCH_X86, UC_MODE_32)
mu.mem_map(0, len(ram))
mu.mem_write(0, ram)
for register, value in zip(REGISTERS, values[1:9]):
    mu.reg_write(register, value)
mu.reg_write(UC_X86_REG_EFLAGS, values[9])
if args.initial:
    mu.reg_write(UC_X86_REG_GDTR, (0, 0x30000, 31, 0))
    mu.reg_write(UC_X86_REG_CR0, 0x11)
    mu.reg_write(UC_X86_REG_CS, 0x10)
    for register in (UC_X86_REG_DS, UC_X86_REG_ES, UC_X86_REG_SS, UC_X86_REG_FS, UC_X86_REG_GS):
        mu.reg_write(register, 0x18)
try:
    mu.emu_start(values[0], 0xffffffff, count=args.steps)
except UcError as error:
    print(f'Oracle stopped: {error}')
    raise SystemExit(1)
actual = [mu.reg_read(UC_X86_REG_EIP)] + [mu.reg_read(r) for r in REGISTERS] + [mu.reg_read(UC_X86_REG_EFLAGS)]
print(' '.join(f'{x:08x}' for x in actual))
if args.compare:
    expected = [int(x, 16) for x in Path(args.compare + '.regs').read_text().split()]
    if actual != expected:
        raise SystemExit(f'Register mismatch: expected {expected}')
    if bytes(mu.mem_read(0, len(ram))) != Path(args.compare + '.ram').read_bytes():
        raise SystemExit('RAM mismatch')
    print('All registers and RAM match')
