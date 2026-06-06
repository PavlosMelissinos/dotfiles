#!/usr/bin/env nix-shell
#!nix-shell -i python3 -p python3
"""Regenerate docs/proposals/README.md from YAML frontmatter in PROP-*.md files.

Scans PROP-*.md files in both the base directory and ./archive/, reads ---
delimited YAML frontmatter, and writes README.md with index tables for
active and archived proposals sorted by proposal number.
"""

import glob
import pathlib
import re

DIR = pathlib.Path(__file__).resolve().parent
README = DIR / "README.md"
ARCHIVE = DIR / "archive"


def parse_frontmatter(text: str) -> dict | None:
    m = re.match(r"^---\n(.*?)\n---", text, re.DOTALL)
    if not m:
        return None
    meta = {}
    for line in m.group(1).splitlines():
        if ": " in line:
            key, val = line.split(": ", 1)
            meta[key.strip()] = val.strip().strip('"')
    return meta


def status_icon(status: str) -> str:
    icons = {
        "Proposed":     "🔵",
        "In Progress":  "🟡",
        "Completed":    "✅",
        "Rejected":     "🔴",
        "Superseded":   "⚫",
        "Accepted":     "🟢",
    }
    return icons.get(status, "⚪")


def build_table(proposals: list[tuple[str, bool, dict]]) -> str:
    rows = ""
    for f, is_archive, m in proposals:
        icon  = status_icon(m["status"])
        title = m["title"]
        date  = m["date"]
        cat   = m.get("category", "")
        if is_archive:
            link = f"[{f}](./archive/{f})"
        else:
            link = f"[{f}](./{f})"
        rows += f"| {link} | {title} | {icon} {m['status']} | {date} | {cat} |\n"
    return rows


def build_readme(active: list, archived: list) -> str:
    active_rows    = build_table(active)
    archived_rows  = build_table(archived)

    return f"""# Proposals

This directory contains proposals for changes to this dotfiles repository.

## Active Proposals ({len(active)})

| Proposal                                              | Title               | Status              | Date       | Category |
|-------------------------------------------------------|---------------------|---------------------|------------|----------|
{active_rows}
## Archive ({len(archived)})

| Proposal                                              | Title               | Status              | Date       | Category |
|-------------------------------------------------------|---------------------|---------------------|------------|----------|
{archived_rows}
## Workflow

### Creating a new proposal

```bash
cp TEMPLATE.md PROP-NNNN-slug.md
$EDITOR PROP-NNNN-slug.md       # fill frontmatter, decompose into tasks
python3 gen-index.py            # auto-rebuild README.md
git add PROP-NNNN-slug.md       # README.md is gitignored
git commit -m "[docs] Add proposal: <title>"
```

### Tracking progress

Every proposal contains a ``## Tasks`` section with granular markdown
checklists (``- [ ]`` / ``- [x]``). The status lifecycle is:

```
Proposed → In Progress → Accepted (done)
                ↓
            Rejected / Superseded
```

1. **Pick up a proposal**: change frontmatter ``status: Proposed`` →
   ``status: In Progress``, add a ``## Tasks`` checklist if missing.
2. **Work**: commit each completed task, checking ``- [ ]`` → ``- [x]``.
3. **Finish**: when all tasks are checked, change status to ``Accepted`` and
   run ``python3 gen-index.py`` (moves it to the archive section).
4. **Discard**: if rejected or superseded, set the terminal status and
   regenerate the index.
"""


def main():
    proposals = []

    for f in sorted(glob.glob("PROP-*.md", root_dir=DIR)):
        text = (DIR / f).read_text()
        meta = parse_frontmatter(text)
        if meta:
            proposals.append((f, False, meta))

    if ARCHIVE.exists():
        for f in sorted(ARCHIVE.glob("PROP-*.md")):
            text = f.read_text()
            meta = parse_frontmatter(text)
            if meta:
                proposals.append((f.name, True, meta))

    proposals.sort(
        key=lambda x: int(re.match(r"PROP-(\d+)", x[0]).group(1))
    )

    active   = [(f, a, m) for f, a, m in proposals if m["status"] in ("Proposed", "In Progress")]
    archived = [(f, a, m) for f, a, m in proposals if m["status"] not in ("Proposed", "In Progress")]

    README.write_text(build_readme(active, archived))
    print(f"Wrote {len(active)} active + {len(archived)} archived proposals to {README}")


if __name__ == "__main__":
    main()