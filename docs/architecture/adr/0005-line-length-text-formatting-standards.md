# ADR-0005: Line Length and Text Formatting Standards

**Date**: 2025-07-27
**Status**: Accepted

## Context

Text files in this repository currently have inconsistent line lengths,
with some lines exceeding 300 characters. Long lines create several
problems:

- Poor readability on narrow screens, mobile devices, and split-screen
  setups
- Difficulty in code review tools that don't wrap lines effectively
- Challenges when viewing diffs and comparing changes
- Inconsistent formatting across different editors and viewers
- Harder to scan and read technical documentation

The 80-character line limit has been a long-standing convention in
software development, originating from terminal limitations but
remaining relevant for readability. Modern alternatives include 72
characters (git commit body), 79 characters (Python PEP 8), or 100
characters (some modern style guides).

Files affected include:
- Documentation (README.org, CLAUDE.md, ADRs)
- Configuration files
- Code comments and documentation

## Decision

We will enforce an 80-character line length limit for all text files
in this repository.

Line wrapping rules:
- Hard wrap at 80 characters for prose and documentation
- Break lines at natural boundaries (spaces, punctuation)
- Indent continuation lines appropriately for readability
- Exception: URLs and code examples that cannot be broken
- Exception: Tables and structured data where wrapping breaks formatting

Files covered:
- All Markdown files (*.md)
- Org-mode files (*.org)
- Configuration files where applicable
- Code comments and documentation strings

## Consequences

**Positive:**
- Improved readability across all devices and screen sizes
- Better experience in code review tools and diff viewers
- Consistent formatting that works well with version control
- Easier scanning and reading of technical documentation
- Better compatibility with terminal-based editors and viewers
- Consistent experience across different development environments

**Negative:**
- Some sentences may need to be restructured or broken awkwardly
- Tables and structured content may require more complex formatting
- URLs and long technical terms may need special handling
- Additional effort required to maintain line length during editing
- Some automatic text generation may need adjustment