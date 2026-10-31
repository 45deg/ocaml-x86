#!/usr/bin/env python3
"""Check guest output from fixtures/alpine/shell-smoke.txt, not echoed commands."""
import argparse
import pathlib
import re


def check(text):
    text = re.sub(r"\x1b\[[0-?]*[ -/]*[@-~]", "", text).replace("\r", "")
    lines = {line.strip() for line in text.splitlines()}
    expected = {"OCAML_OS_READY", "OCAML_SHELL_OK", "arithmetic=42", "i686", "3.22.7", "OCAML_SHELL_DONE"}
    missing = sorted(expected - lines)
    if "Run /init as init process" not in text:
        missing.append("kernel exec of /init")
    return missing


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("log")
    args = parser.parse_args()
    missing = check(pathlib.Path(args.log).read_text())
    if missing:
        parser.exit(1, "Missing guest results: " + ", ".join(missing) + "\n")
    print("Verified Alpine 3.22.7 BusyBox shell: arithmetic, external command, filesystem read.")
