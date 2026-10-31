#!/usr/bin/env python3
"""Refresh project sources in a prepared self-compilation toolchain rootfs."""
import argparse
import gzip
import hashlib
import io
import json
from pathlib import Path
import tarfile

from make_initramfs import build


def prepare(mode, source, output):
    repo = Path(__file__).resolve().parent.parent
    root = "native" if mode == "native" else "selfcompile"
    if not source.is_file():
        raise ValueError(
            f"Missing prepared {mode} toolchain: {source}. "
            "Set SELF_COMPILE_ROOTFS to a compatible rootfs.tar.gz; see README.md."
        )
    if source.resolve() == (output / "rootfs.tar.gz").resolve():
        raise ValueError("The input toolchain rootfs must differ from the generated rootfs.")
    sources = {
        f"{root}/src/{path.relative_to(repo)}": path.read_bytes()
        for directory in ("lib", "bin")
        for path in sorted((repo / directory).glob("*.ml"))
    }
    replacements = dict(sources)
    replacements["self-compile-guest.sh"] = (
        repo / "scripts/self_compile_guest.sh"
    ).read_bytes()
    replacements["self-compile-start.sh"] = (
        "#!/bin/sh\n"
        "mount -t proc proc /proc\n"
        "mount -t sysfs sysfs /sys\n"
        f"/bin/sh /self-compile-guest.sh {mode}\n"
        "status=$?\n"
        "echo SELF_COMPILE_SCRIPT_EXIT=$status\n"
        "echo SELF_COMPILE_FINISHED\n"
        "exec /bin/sh -i\n"
    ).encode()
    required = {f"{root}/hello.bin", f"{root}/stdlib/stdlib.cmi"}
    if mode == "native":
        required.update({"native/ocamlopt.opt", "native/tools/cc", "native/tools/link"})
    else:
        required.update({"usr/bin/ocamlrun", "selfcompile/ocamlc.byte"})
    # Keep the original evidence image intact; remove stale sources and outputs.
    output.mkdir(parents=True, exist_ok=True)
    with tarfile.open(source) as original, tarfile.open(
        output / "rootfs.tar.gz", "w:gz", compresslevel=1
    ) as target:
        for member in original:
            name = str(Path(member.name))
            required.discard(name)
            if name in replacements or (
                not member.isdir()
                and name.startswith((f"{root}/src/", f"{root}/work/"))
            ):
                continue
            target.addfile(member, original.extractfile(member) if member.isfile() else None)
        if required:
            raise ValueError(f"Incompatible toolchain rootfs; missing: {', '.join(sorted(required))}")
        for name, data in replacements.items():
            member = tarfile.TarInfo(name)
            member.size = len(data)
            member.mode = 0o755 if name.endswith(".sh") else 0o644
            target.addfile(member, io.BytesIO(data))
    (output / "initramfs.cpio.gz").write_bytes(
        gzip.compress(build(output / "rootfs.tar.gz"), compresslevel=1, mtime=0)
    )
    (output / "source-hashes.json").write_text(
        json.dumps(
            {name: hashlib.sha256(data).hexdigest() for name, data in sources.items()},
            indent=2,
        ) + "\n"
    )
    print(f"Prepared {mode}: {len(sources)} current source files -> {output}/initramfs.cpio.gz")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=("native", "ocamlrun"))
    parser.add_argument("rootfs", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    try:
        prepare(args.mode, args.rootfs, args.output)
    except (ValueError, FileNotFoundError) as error:
        parser.exit(1, f"{error}\n")
