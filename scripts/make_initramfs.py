#!/usr/bin/env python3
"""Convert a rootfs tarball to deterministic Linux newc initramfs.

No archive paths or device nodes are extracted onto the host filesystem.
"""
import argparse
import gzip
import io
import pathlib
import stat
import tarfile


def build(source):
    output = io.BytesIO()
    inode = 0

    def add(name, mode, data=b"", major=0, minor=0):
        nonlocal inode
        inode += 1
        name = name.encode() + b"\0"
        fields = [inode, mode, 0, 0, 1, 0, len(data), 0, 0, major, minor, len(name), 0]
        output.write(b"070701" + b"".join(f"{value:08x}".encode() for value in fields))
        output.write(name)
        output.write(b"\0" * (-output.tell() % 4))
        output.write(data)
        output.write(b"\0" * (-output.tell() % 4))

    with tarfile.open(source, "r:gz") as archive:
        seen = set()
        for member in archive:
            path = pathlib.PurePosixPath(member.name)
            if path.is_absolute() or ".." in path.parts:
                raise ValueError(f"Unsafe archive path: {member.name}")
            name = str(path)
            if name in {"init", "dev/console", "dev/null", "dev/ttyS0"}:
                continue
            if name in seen:
                raise ValueError(f"Duplicate archive path: {name}")
            seen.add(name)
            if member.isdir():
                add(name, stat.S_IFDIR | member.mode)
            elif member.issym():
                add(name, stat.S_IFLNK | member.mode, member.linkname.encode())
            elif member.isfile() or member.islnk():
                with archive.extractfile(member) as stream:
                    add(name, stat.S_IFREG | member.mode, stream.read())
            else:
                raise ValueError(f"Unsupported archive member: {name}")
    if "dev" not in seen:
        add("dev", stat.S_IFDIR | 0o755)
    add("dev/console", stat.S_IFCHR | 0o600, major=5, minor=1)
    add("dev/null", stat.S_IFCHR | 0o666, major=1, minor=3)
    add("dev/ttyS0", stat.S_IFCHR | 0o600, major=4, minor=64)
    add("init", stat.S_IFREG | 0o755, b"#!/bin/sh\nmount -t proc proc /proc\nmount -t sysfs sysfs /sys\necho OCAML_OS_READY\nexec /bin/sh -i\n")
    add("TRAILER!!!", 0)
    return output.getvalue()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("rootfs")
    parser.add_argument("output")
    parser.add_argument("--uncompressed", action="store_true", help="Write raw newc cpio")
    args = parser.parse_args()
    image = build(args.rootfs)
    pathlib.Path(args.output).write_bytes(
        image if args.uncompressed else gzip.compress(image, mtime=0)
    )
