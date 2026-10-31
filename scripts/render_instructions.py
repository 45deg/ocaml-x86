#!/usr/bin/env python3
"""Render the static instruction table as an SVG for the README (stdlib only)."""

from html import escape
import json
from pathlib import Path


STATES = {"yes": ("#d9eddf", "#214c32"),
          "partial": ("#fff0b8", "#584400"),
          "no": ("#e7e9eb", "#424950")}
ROOT = Path(__file__).resolve().parents[1]
data = json.loads((ROOT / "docs/instructions.json").read_text())
groups = [(category["category"],
           [(batch["status"], label, batch["note"])
            for batch in category["groups"] for label in batch["instructions"]])
          for category in data]
width, columns, cell_width, pitch, row_height = 1000, 8, 100, 104, 24
height = 60 + sum(((len(items) + columns - 1) // columns) * row_height + 12
                  for _, items in groups)
svg = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" '
       f'viewBox="0 0 {width} {height}" role="img" aria-labelledby="title desc">',
       '<title id="title">ocaml-x86 命令対応表</title>',
       '<desc id="desc">カテゴリ別の命令グリッド。✓ 緑は対応、△ 黄は部分対応、'
       '— グレーは未対応。保護モードの8/16/32-bitが対象。</desc>',
       f'<rect width="{width}" height="{height}" fill="white"/>',
       '<style>text{font-family:system-ui,sans-serif;fill:#222}'
       '.instruction{font-family:ui-monospace,Menlo,Consolas,monospace;font-size:11px}'
       '.category{font-size:12px;font-weight:600}</style>',
       '<text x="12" y="24" font-size="18" font-weight="600">命令対応表</text>']
for x, state, label in [(650, "yes", "✓ 対応"), (750, "partial", "△ 部分対応"),
                         (882, "no", "— 未対応")]:
    bg, fg = STATES[state]
    svg.append(f'<rect x="{x}" y="8" width="{112 if state == "partial" else 92}" height="24" fill="{bg}"/>'
               f'<text x="{x + 8}" y="24" font-size="12" style="fill:{fg}">{label}</text>')
svg.append('<text x="12" y="47" font-size="11">保護モード 8/16/32-bit</text>')
y = 60
for category, items in groups:
    svg.append(f'<path d="M12 {y - 4}H988" stroke="#ddd"/>')
    # Wrap long Japanese category labels without depending on foreignObject.
    labels = {"文字列・プレフィックス": ["文字列・", "プレフィックス"],
              "x87 転送・スタック": ["x87 転送・スタック"]}.get(category, [category])
    for line, label in enumerate(labels):
        svg.append(f'<text class="category" x="12" y="{y + 15 + line * 17}">{escape(label)}</text>')
    for index, (state, label, note) in enumerate(items):
        x, top = 160 + (index % columns) * pitch, y + (index // columns) * row_height
        bg, fg = STATES[state]
        fit = ' textLength="90" lengthAdjust="spacingAndGlyphs"' if len(label) > 14 else ""
        svg.append(f'<g><title>{escape(note)}</title>'
                   f'<rect x="{x}" y="{top}" width="{cell_width}" height="21" fill="{bg}"/>'
                   f'<text class="instruction" x="{x + 5}" y="{top + 14}" style="fill:{fg}"{fit}>{escape(label)}</text></g>')
    y += ((len(items) + columns - 1) // columns) * row_height + 12
svg.append('</svg>')
(ROOT / "docs/instructions.svg").write_text("\n".join(svg) + "\n")
