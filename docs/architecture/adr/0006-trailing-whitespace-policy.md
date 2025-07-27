# ADR-0006: Trailing Whitespace Policy

**Date**: 2025-07-27
**Status**: Accepted

## Context

Trailing whitespace refers to space or tab characters at the end of
lines that serve no purpose. These invisible characters create several
problems:

- Git shows trailing whitespace as errors in diffs and highlights
- Inconsistent behavior across different editors and viewers
- Unnecessary noise in version control commits and diffs
- Poor collaboration experience when team members have different
  whitespace settings
- Files appear "dirty" in editors that highlight trailing whitespace
- Potential issues with automated processing and parsing tools

Currently, some files in this repository contain trailing whitespace,
particularly in documentation files like CLAUDE.md. This creates
inconsistent formatting and unnecessary git diff noise.

Most modern editors can be configured to:
- Highlight trailing whitespace visually
- Automatically remove trailing whitespace on save
- Show invisible characters for manual review

## Decision

We will maintain a zero-tolerance policy for trailing whitespace in
all text files.

Implementation:
- Remove all existing trailing whitespace from repository files
- Configure development tools to prevent introduction of new trailing
  whitespace
- Exception: Markdown files where two trailing spaces create line
  breaks (but this should be avoided in favor of explicit breaks)
- Exception: Files where trailing whitespace has semantic meaning
  (rare in this repository)

Files covered:
- All source code files
- All documentation files (*.md, *.org)
- Configuration files
- Any other text-based files in the repository

## Consequences

**Positive:**
- Clean git diffs without whitespace noise
- Consistent formatting across all files and editors
- Better collaboration experience for all contributors
- Professional appearance of code and documentation
- Easier automated processing and parsing of text files
- Reduced confusion from invisible characters

**Negative:**
- Requires attention during editing to avoid introducing trailing
  whitespace
- May require editor configuration changes for optimal workflow
- Occasional need to be mindful of Markdown line break syntax
- One-time effort required to clean existing files