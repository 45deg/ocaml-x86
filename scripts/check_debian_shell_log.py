#!/usr/bin/env python3
"""Verify actual guest output from Debian installer or slim shell smoke inputs."""
import argparse
import pathlib
import re


def check(text, slim=False):
    text = re.sub(r"\x1b\[[0-?]*[ -/]*[@-~]", "", text).replace("\r", "")
    # dash may print the prompt on the same line as buffered command output.
    lines = {
        line.strip().removeprefix("# ") if slim else line.strip()
        for line in text.splitlines()
    }
    expected = {"DEBIAN_SHELL_OK", "arithmetic=42", "6.1.0-50-686", "i686", "DEBIAN_SHELL_DONE"}
    if slim:
        expected = {
            "DEBIAN_SLIM_OK", "arithmetic=42", "6.1.0-50-686",
            "i686", "12.15", "DEBIAN_SLIM_DONE",
        }
    missing = sorted(expected - lines)
    init = "/init" if slim else "/bin/sh"
    if f"Run {init} as init process" not in text:
        missing.append(f"kernel exec of {init}")
    if slim:
        for mount in ("proc /proc proc ", "sysfs /sys sysfs "):
            if not any(line.startswith(mount) for line in lines):
                missing.append(mount.strip())
    if "Kernel panic" in text or "Initramfs unpacking failed" in text:
        missing.append("boot without panic/unpack error")
    return missing


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("log")
    parser.add_argument("--slim", action="store_true", help="Verify slim-shell-smoke.txt results")
    args = parser.parse_args()
    missing = check(pathlib.Path(args.log).read_text(), slim=args.slim)
    if missing:
        parser.exit(1, "Missing guest results: " + ", ".join(missing) + "\n")
    print("Verified Debian i386 shell: arithmetic, kernel release and external commands.")
