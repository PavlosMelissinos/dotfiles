# Architecture Decision Records (ADRs)

This directory contains Architecture Decision Records for significant
architectural decisions made in this dotfiles repository.

## ADR Format

We follow the standard ADR format from Michael Nygard's "Documenting Architecture Decisions":
- **Title**: Short noun phrase describing the decision
- **Date**: When the decision was made
- **Status**: Proposed, Accepted, Superseded, or Deprecated
- **Context**: Forces at play, technological and project factors (value-neutral)
- **Decision**: Response to forces, written in active voice starting with "We will..."
- **Consequences**: Resulting context after applying the decision (including negative impacts)

## Index

| ADR                                                 | Title                                                      | Status   | Date       |
|-----------------------------------------------------|------------------------------------------------------------|----------|------------|
| [0000](./0000-record-architecture-decisions.md)     | Record Architecture Decisions                              | Accepted | 2025-07-27 |
| [0001](./0001-package-manager-consolidation.md)     | Complete Package Manager Consolidation                     | Accepted | 2025-07-27 |
| [0002](./0002-sway-window-manager-wayland-first.md) | Sway Window Manager with Wayland-First Desktop Environment | Accepted | 2025-07-27 |
| [0003](./0003-xdg-directory-compliance.md)          | XDG Base Directory Specification Compliance                | Accepted | 2025-07-27 |
| [0004](./0004-u2f-hardware-key-authentication.md)   | U2F Hardware Key Authentication                            | Accepted | 2025-07-27 |

## ADR Process

1. **Propose**: Create new ADR with "Proposed" status
2. **Review**: Discuss with stakeholders (if applicable)
3. **Decide**: Update status to "Accepted", "Superseded", or "Deprecated"
4. **Implement**: Execute the decision
5. **Update**: Reflect implementation results in the ADR

## Templates

Use this template for new ADRs:

```markdown
# ADR-XXXX: [Short noun phrase describing the decision]

**Date**: YYYY-MM-DD
**Status**: [Proposed/Accepted/Superseded/Deprecated]

## Context
[Forces at play, technological and project factors - value-neutral description]

## Decision
We will [response to forces, written in active voice]...

## Consequences
[Resulting context after applying the decision, including ALL consequences]

**Positive:**
- [Good outcomes]

**Negative:**
- [Challenges or trade-offs]
```
