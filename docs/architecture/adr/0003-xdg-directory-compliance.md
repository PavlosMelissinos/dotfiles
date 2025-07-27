# ADR-0003: XDG Base Directory Specification Compliance

**Date**: 2025-07-27
**Status**: Accepted

## Context

Many applications create configuration files, caches, and data directly in the
user's home directory, leading to clutter. The XDG Base Directory Specification
defines standard locations:
- `$XDG_CONFIG_HOME` (~/.config): User-specific configuration files
- `$XDG_CACHE_HOME` (~/.cache): User-specific non-essential cached data
- `$XDG_DATA_HOME` (~/.local/share): User-specific data files
- `$XDG_STATE_HOME` (~/.local/state): User-specific state data

Benefits of compliance:
- Cleaner home directory organization
- Easier backup and synchronization (separate config from cache)
- Better integration with modern file managers
- Consistent application behavior

Challenges:
- Not all applications respect XDG directories
- Some applications require explicit configuration to use XDG paths
- Legacy applications may not support XDG at all

## Decision

We will configure all applications to use XDG Base Directory Specification when
possible.

For applications that support XDG natively, we will set appropriate environment
variables:

```bash
XDG_CONFIG_HOME="$HOME/.config"
XDG_CACHE_HOME="$HOME/.cache"
XDG_DATA_HOME="$HOME/.local/share"
XDG_STATE_HOME="$HOME/.local/state"
```

For applications that require explicit configuration:
- Emacs: Configure to use XDG directories instead of ~/.emacs.d
- Zsh: Use dotDir configuration to place files in ~/.config/zsh
- Tmux: Set TMUX_HOME to use ~/.config/tmux

For applications that don't support XDG, we will document the limitation and
consider alternatives when possible.

## Consequences

**Positive:**
- Cleaner home directory with organized subdirectories
- Easier backup strategies (can exclude cache directories)
- Better separation of configuration from runtime data
- Consistent file organization across applications
- Easier dotfiles management and synchronization

**Negative:**
- Additional configuration complexity for non-compliant applications
- Some applications (Firefox, Thunderbird, Kodi) cannot be made compliant
- May require wrapper scripts or environment variable manipulation
- Debugging may be more complex when files are not in expected locations
- Migration of existing application data required
