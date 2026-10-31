#!/usr/bin/env python3
"""Extract a gzip or XZ ELF payload from a Linux bzImage for development boots.
Does not patch the kernel; fails for other compression formats.
"""
import argparse
import lzma
import struct
import zlib
from pathlib import Path

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('bzimage', type=Path)
parser.add_argument('output', type=Path)
args = parser.parse_args()
data = args.bzimage.read_bytes()
if len(data) < 0x250 or data[0x202:0x206] != b'HdrS':
    parser.error('Not a modern bzImage')
setup = ((data[0x1f1] or 4) + 1) * 512
start, size = struct.unpack_from('<II', data, 0x248)
payload = data[setup + start:setup + start + size]
if len(payload) != size:
    parser.error('Truncated compressed payload')
if payload.startswith(b'\x1f\x8b'):
    image = zlib.decompress(payload, 31)
elif payload.startswith(b'\xfd7zXZ\x00'):
    decompressor = lzma.LZMADecompressor()
    image = decompressor.decompress(payload)
    if not decompressor.eof:
        parser.error('Incomplete XZ stream')
else:
    parser.error('Expected a gzip or XZ payload')
if image[:6] != b'\x7fELF\x01\x01':
    parser.error('Payload is not ELF32 little-endian')
args.output.write_bytes(image)
print(f'Extracted {len(image)} bytes to {args.output}')
