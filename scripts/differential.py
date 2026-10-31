#!/usr/bin/env python3
"""Optional, test-only Unicorn oracle. The emulator itself never links Unicorn.
Run after dune build test/instruction_probe.exe, with Python unicorn==2.1.4.
"""
import random
import subprocess
import sys
from pathlib import Path
from unicorn import Uc, UcError, UC_ARCH_X86, UC_MODE_32
from unicorn.x86_const import (
    UC_X86_REG_EAX, UC_X86_REG_ECX, UC_X86_REG_EDX, UC_X86_REG_EBX,
    UC_X86_REG_ESP, UC_X86_REG_EBP, UC_X86_REG_ESI, UC_X86_REG_EDI,
    UC_X86_REG_EFLAGS, UC_X86_REG_EIP,
)

ROOT = Path(__file__).resolve().parents[1]
REGISTERS = [UC_X86_REG_EAX, UC_X86_REG_ECX, UC_X86_REG_EDX, UC_X86_REG_EBX,
             UC_X86_REG_ESP, UC_X86_REG_EBP, UC_X86_REG_ESI, UC_X86_REG_EDI]
RNG = random.Random(0x386)
MEMORY = bytes((i * 37 + 11) & 255 for i in range(512))


def cases():
    for width in (8, 16, 32):
        prefix = b'\x66' if width == 16 else b''
        odd = width != 8
        for _ in range(100):
            for group in range(8):
                # Arithmetic register/memory, both directions, all widths.
                for direction in (0, 2):
                    opcode = group * 8 + direction + odd
                    modrm = RNG.choice([0xc1, 0xd0, 0xe3, 0x03, 0x4b])
                    tail = b'\x07' if modrm == 0x4b else b''
                    yield prefix + bytes([opcode, modrm]) + tail, 0x8d5, 'alu'
            for group in range(8):
                count = RNG.randrange(32)
                code = prefix + bytes([0xc0 + odd, 0xc0 | group << 3, count])
                effective = count % (width if group < 2 else width + 1) if group < 4 else count
                mask = 0x8d5 if count == 0 or (group in (2, 3) and effective == 0) else 1
                if group >= 4:
                    mask |= 0xc4
                    if count >= width and group != 7:
                        mask &= ~1  # CF undefined for SHL/SHR count >= width.
                if count == 1:
                    mask |= 0x800
                yield code, mask, 'shift'
            for group in (4, 5, 6, 7):
                yield prefix + bytes([0xf6 + odd, 0xc1 | group << 3]), 0x801 if group < 6 else 0, 'muldiv'
            if odd:
                yield prefix + b'\x0f\xaf\xc1', 0x801, 'imul'
                yield prefix + b'\x6b\xc1' + bytes([RNG.randrange(256)]), 0x801, 'imul-imm'
                for opcode in (0xa4, 0xac):
                    count = RNG.randrange(width + 1)
                    mask = 0x8d5 if count == 0 else 0xc5 | (0x800 if count == 1 else 0)
                    yield prefix + bytes([0x0f, opcode, 0xc8, count]), mask, 'double-shift'
                for opcode in (0xa3, 0xab, 0xb3, 0xbb):
                    yield prefix + bytes([0x0f, opcode, 0xc8]), 1, 'bit'
                for opcode in (0xbc, 0xbd):
                    yield prefix + bytes([0x0f, opcode, 0xc1]), 64, 'scan'
                for condition in range(16):
                    yield prefix + bytes([0x0f, 0x40 + condition, 0xc1]), 0x8d5, 'cmov'
            for opcode in (0xb0 + odd, 0xc0 + odd):
                yield prefix + bytes([0x0f, opcode, 0xcb]), 0x8d5, 'cmpxchg-xadd'


    for _ in range(200):
        for prefix in (b'', b'\x66'):
            for code in (b'\x60', b'\x61', b'\xc9', b'\xc8\x10\x00\x00', b'\xc8\x10\x00\x03',
                         b'\x87\xcb', b'\x87\x03', b'\x8f\x04\x24', b'\x0f\xc8',
                         b'\x8b\x44\x33\x07', b'\x8b\x04\x25\x00\x40\x00\x00',
                         b'\x67\x8b\x40\xfe', b'\x67\x8b\x06\x00\x40', b'\xd7'):
                if prefix and code == b'\x0f\xc8':
                    continue  # 16-bit BSWAP is architecturally undefined.
                yield prefix + code, 0x8d5, 'stack-address-transfer'
            for group in range(8):
                for opcode in (0x81, 0x83):
                    immediate = RNG.getrandbits(16 if prefix else 32).to_bytes(2 if prefix else 4, 'little') if opcode == 0x81 else bytes([RNG.randrange(256)])
                    yield prefix + bytes([opcode, 0xc0 | group << 3]) + immediate, 0x8d5, 'group1-immediate'


def main():
    process = subprocess.Popen([str(ROOT / '_build/default/test/instruction_probe.exe')],
                               stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
    checked = 0
    try:
        for code, mask, label in cases():
            values = [RNG.getrandbits(32) for _ in range(8)]
            values[3:8] = [0x4000, 0xe000, 0x4100, 0x4040, 0x4080]
            flags = 2 | (RNG.getrandbits(12) & 0x8d5)
            mu = Uc(UC_ARCH_X86, UC_MODE_32)
            mu.mem_map(0, 65536)
            mu.mem_write(0x1006, code)
            mu.mem_write(0x4000, MEMORY)
            for register, value in zip(REGISTERS, values):
                mu.reg_write(register, value)
            mu.reg_write(UC_X86_REG_EFLAGS, flags)
            status = 'ok'
            try:
                mu.emu_start(0x1006, 0xffff, count=1)
            except UcError:
                status = 'fault'
            line = ' '.join([code.hex(), hex(flags)] + [hex(x) for x in values])
            process.stdin.write(line + '\n')
            process.stdin.flush()
            output = process.stdout.readline().split()
            if not output:
                raise RuntimeError('Probe terminated')
            actual = [int(x, 16) for x in output[1:11]]
            expected = [mu.reg_read(UC_X86_REG_EIP)] + [mu.reg_read(r) for r in REGISTERS] + [mu.reg_read(UC_X86_REG_EFLAGS)]
            valid = output[0] == status
            if status == 'ok':
                valid &= actual[:9] == expected[:9] and (actual[9] & mask) == (expected[9] & mask)
                valid &= bytes.fromhex(output[11]) == bytes(mu.mem_read(0x4000, 512))
            else:
                valid &= actual[0] == 0x1006 and actual[1:9] == values
            if not valid:
                raise AssertionError(f'{label} {line}\nactual={output[:11]}\nexpected={status} {[hex(x) for x in expected]} mask={mask:x}')
            checked += 1
        print(f'{checked} differential instruction cases passed (seed=0x386)')
    finally:
        process.stdin.close()
        process.wait()


if __name__ == '__main__':
    main()
