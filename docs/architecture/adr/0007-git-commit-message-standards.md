# ADR-0007: Git Commit Message Standards

**Date**: 2025-07-27
**Status**: Accepted

## Context

Git commit messages serve as the primary documentation of project
evolution and are crucial for:

- Understanding why changes were made (not just what changed)
- Navigating project history effectively
- Code review and collaboration
- Automated tools that parse commit messages
- Future debugging and maintenance

Currently, commit messages in this repository lack consistency in
format, style, and content quality. Some commits use past tense,
others use present tense, and many lack sufficient context about the
reasoning behind changes.

Chris Beams' article "How to Write a Git Commit Message" provides
widely-accepted industry standards for commit message formatting.
These standards improve collaboration and maintainability.

## Decision

We will enforce standardized git commit message format following the
seven rules from Chris Beams' guide.

Required format:
1. Separate subject from body with a blank line
2. Limit subject line to 50 characters
3. Capitalize the subject line
4. Do not end the subject line with a period
5. Use imperative mood in the subject line
6. Wrap body text at 72 characters
7. Use the body to explain "what" and "why", not "how"

Subject line test: "If applied, this commit will [subject line]"

Standard format:
```
Add feature X to improve Y

Explain the context and reasoning behind the change.
Reference ADR-XXX if this involves architectural decisions.
Include any relevant background information.

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>
```

Imperative verbs to use:
- Add (new features, files, functionality)
- Fix (bugs, issues, problems)
- Update (existing features, documentation)
- Remove (obsolete code, files, features)
- Refactor (code structure without changing behavior)

## Consequences

**Positive:**
- Consistent, professional commit history
- Easier navigation and understanding of project evolution
- Better collaboration with clear change explanations
- Improved debugging through better historical context
- Compatibility with automated tools and scripts
- Professional development practices

**Negative:**
- Requires more thought and time when making commits
- Need to break habits if using different commit styles previously
- Occasionally challenging to fit complex changes into 50 characters
- Additional effort to explain reasoning rather than just listing
  changes