# Architecture Decision Records (ADRs)

This directory contains Architecture Decision Records for significant architectural decisions made in this dotfiles repository.

## ADR Format

We follow the standard ADR format:
- **Status**: Proposed, Accepted, Superseded, or Deprecated
- **Date**: When the decision was made
- **Context**: The situation that necessitated the decision
- **Decision**: What was decided
- **Consequences**: The positive and negative impacts

## Index

| ADR | Title | Status | Date |
|-----|-------|--------|------|
| [0001](./0001-package-manager-consolidation.md) | Complete Package Manager Consolidation | ✅ Accepted | 2025-01-27 |

## ADR Process

1. **Propose**: Create new ADR with "Proposed" status
2. **Review**: Discuss with stakeholders (if applicable)
3. **Decide**: Update status to "Accepted", "Superseded", or "Deprecated"
4. **Implement**: Execute the decision
5. **Update**: Reflect implementation results in the ADR

## Templates

Use this template for new ADRs:

```markdown
# ADR-XXXX: [Title]

**Status**: [Proposed/Accepted/Superseded/Deprecated]  
**Date**: YYYY-MM-DD  
**Deciders**: [Who made the decision]  

## Context
[The situation that necessitated the decision]

## Decision
[What was decided]

## Consequences
### Positive
- [Good outcomes]

### Negative  
- [Challenges or trade-offs]

## Implementation Details
[How the decision was implemented]

## References
[Links to related documents, commits, etc.]
```