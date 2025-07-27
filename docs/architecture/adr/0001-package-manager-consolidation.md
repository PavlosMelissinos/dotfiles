# ADR-0001: Complete Package Manager Consolidation

**Status**: ✅ Accepted and Implemented
**Date**: 2025-01-27
**Deciders**: System Administrator

## Context

The system had fragmented package management across multiple managers:
- **DNF/RPM**: 1,993 system packages (Fedora base)
- **home-manager/Nix**: 38 user packages
- **Guix**: 81 user packages
- **Flatpak**: 2 applications

This led to:
- PATH priority conflicts (Guix took precedence over Nix)
- Duplicate binaries causing tool confusion
- Complex maintenance with 4+ package managers
- Split configuration management

## Decision

**Consolidate ALL user package management under home-manager/Nix, completely eliminating Guix.**

### Migration Strategy
1. **Stage 1**: Migrate development tools (rust, cmake, meson, etc.)
2. **Stage 2**: Migrate desktop environment (Wayland/Sway components)
3. **Stage 3**: Migrate remaining applications and eliminate overlaps
4. **Stage 4**: Complete Guix elimination and PATH cleanup

### Target Architecture
```
┌─ USER SPACE ────────────────────────────────────┐
│  home-manager/Nix (PRIMARY & ONLY)              │
│  ├─ Development Tools (rust, node, python...)   │
│  ├─ Desktop Environment (sway, waybar, mako...) │
│  ├─ Applications (firefox, vlc, steam...)       │
│  ├─ System Libraries (fonts, certificates...)   │
│  └─ Configuration Management (git, zsh, emacs)  │
└─────────────────────────────────────────────────┘
┌─ SYSTEM SPACE ──────────────────────────────────┐
│  DNF/RPM (System packages only)                 │
│  ├─ Kernel and drivers                          │
│  ├─ System services (systemd, etc.)             │
│  ├─ Core libraries (glibc, etc.)                │
│  └─ Hardware firmware                           │
└─────────────────────────────────────────────────┘
┌─ SANDBOXED ─────────────────────────────────────┐
│  Flatpak (Minimal - special cases only)         │
│  ├─ Element (Matrix client)                     │
│  └─ DBeaver Community                           │
└─────────────────────────────────────────────────┘
```

## Consequences

### Positive
- ✅ **Eliminated conflicts**: No duplicate binaries across managers
- ✅ **Simplified maintenance**: Single configuration file (`home.nix`)
- ✅ **Better reproducibility**: Complete user environment in version control
- ✅ **Faster operations**: No package manager coordination needed
- ✅ **Enhanced integration**: Unified package + configuration management
- ✅ **Clean PATH**: Nix binaries take highest precedence

### Negative
- ⚠️ **Learning curve**: Team needs to understand Nix package names
- ⚠️ **Package availability**: Some packages may have different names in Nixpkgs
- ⚠️ **Rollback complexity**: Must understand Nix generations for rollbacks

### Implementation Results
- **Before**: 38 Nix + 81 Guix packages (119 total, with duplicates)
- **After**: 95+ Nix packages (complete coverage, zero duplicates)
- **Guix manifest**: Empty - `(specifications->manifest (list))`
- **PATH management**: Clean, Nix-first priority

## Implementation Details

### Key Package Migrations
- **Development**: `rust` → `rustc` + `cargo`, `cmake`, `meson`, `openssl`
- **Desktop**: All Wayland/Sway components, media applications
- **System**: Fonts, certificates, locales moved to Nix equivalents
- **Conflicts resolved**: `bsd-games` vs `fortune`, `thunar` → `xfce.thunar`

### Configuration Changes
- **home.nix**: Expanded to 95+ packages with proper categories
- **Guix manifest**: Emptied completely
- **PATH**: `$HOME/.nix-profile/bin` has highest priority
- **Environment**: Removed all Guix references

### Verification
- All 95+ packages functional from Nix
- 312 binaries available in Nix profile
- Desktop environment fully operational
- Development tools properly integrated

## References

- [Complete Guix Elimination Report](../../../complete-guix-elimination-report.md)
- [Package Manager Consolidation Report](../../../package-manager-consolidation-report.md)
- Implementation commit: b20a2de
