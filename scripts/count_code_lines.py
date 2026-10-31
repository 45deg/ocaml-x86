#!/usr/bin/env python3
"""Print the README code-size row for implementation sources, excluding blank lines."""

from pathlib import Path
import re

ROOT = Path(__file__).resolve().parents[1]

with_comments = without_comments = 0
for path in sorted([*(ROOT / 'lib').glob('*.ml'), *(ROOT / 'bin').glob('*.ml')]):
    source = path.read_text()
    stripped = list(source)
    depth = 0
    # Skip string literals and recognize nested OCaml comments.
    tokens = r'"(?:\\.|[^"\\])*"|\{([a-z_]*)\|.*?\|\1\}|\(\*|\*\)'
    for match in re.finditer(tokens, source, re.S):
        token = match.group()
        if token == '(*':
            if depth == 0:
                start = match.start()
            depth += 1
        elif token == '*)' and depth:
            depth -= 1
            if depth == 0:
                end = match.end()
                stripped[start:end] = ['\n' if c == '\n' else ' ' for c in source[start:end]]
    assert depth == 0, f'Unclosed comment: {path}'
    with_comments += sum(bool(line.strip()) for line in source.splitlines())
    without_comments += sum(bool(line.strip()) for line in ''.join(stripped).splitlines())
print(f'| {with_comments:,}行 | {without_comments:,}行 |')
