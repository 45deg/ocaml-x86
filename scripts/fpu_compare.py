#!/usr/bin/env python3
"""Compare finite x87 arithmetic and all rounding modes with test-only Unicorn."""
import random
import struct
import subprocess
from unicorn import Uc, UC_ARCH_X86, UC_MODE_32

rng = random.Random(0x87)
probe = subprocess.Popen(["_build/default/test/fpu_probe.exe"], stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
try:
    for case in range(2400):
        a, b = rng.randrange(-2**31, 2**31), rng.randrange(1, 2**31)
        operation = [0, 1, 4, 5, 6, 7][case % 6]
        control = 0x7f | ([0, 2, 3][(case // 6) % 3] << 8) | (((case // 18) % 4) << 10)
        code = b"\xdb\xe3"  # fninit
        for address, value in [(0x4100, a), (0x4104, b), (0x4108, control)]:
            code += b"\xc7\x05" + struct.pack("<II", address, value & 0xffffffff)
        code += b"\xd9\x2d\x08\x41\x00\x00"  # fldcw
        code += b"\xdb\x05\x00\x41\x00\x00\xdb\x05\x04\x41\x00\x00"  # fild a, b
        code += bytes([0xd8, 0xc1 | (operation << 3)])
        code += b"\xdb\x3d\x00\x40\x00\x00\xf4"  # fstp tbyte, hlt
        oracle = Uc(UC_ARCH_X86, UC_MODE_32)
        oracle.mem_map(0, 65536)
        oracle.mem_write(0x1000, code)
        oracle.emu_start(0x1000, 0x1000 + len(code))
        expected = bytes(oracle.mem_read(0x4000, 10)).hex()
        probe.stdin.write(code.hex() + "\n")
        probe.stdin.flush()
        actual = probe.stdout.readline().strip()
        if actual != expected:
            raise AssertionError((case, a, b, operation, hex(control), actual, expected))
    for case in range(240):
        a, b = rng.randrange(-100, 100), rng.randrange(-100, 100)
        code = b"\xdb\xe3\xbc\x00\x80\x00\x00"
        for address, value in [(0x4100, a), (0x4104, b)]:
            code += b"\xc7\x05" + struct.pack("<II", address, value & 0xffffffff)
        code += b"\xdb\x05\x00\x41\x00\x00\xdb\x05\x04\x41\x00\x00"
        instruction = [b"\xdb\xe9", b"\xdf\xe9", b"\xda\xe9", b"\xdd\xe1", b"\xdd\xe9", b"\xde\xd9"][case % 6]
        code += instruction
        code += b"\x9c\x58" if case % 6 < 2 else b"\xdf\xe0"
        code += b"\xa3\x00\x40\x00\x00\xf4"
        oracle = Uc(UC_ARCH_X86, UC_MODE_32)
        oracle.mem_map(0, 65536)
        oracle.mem_write(0x1000, code)
        oracle.emu_start(0x1000, 0x1000 + len(code))
        expected = bytes(oracle.mem_read(0x4000, 10)).hex()
        probe.stdin.write(code.hex() + "\n")
        probe.stdin.flush()
        actual = probe.stdout.readline().strip()
        if actual != expected:
            raise AssertionError(("compare", case, a, b, actual, expected))
    print("2400 finite arithmetic and 240 x87 comparison cases passed (seed=0x87)")
finally:
    probe.stdin.close()
    probe.wait()
