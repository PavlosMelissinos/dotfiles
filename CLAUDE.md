# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

## Project Overview

This is a personal dotfiles repository for a Linux desktop environment using
Sway (Wayland compositor), managed through home-manager (Nix) and traditional
configuration files. The system is configured for development work with Emacs,
terminal applications, and various GUI tools.

## Package Management & Configuration

### home-manager (Nix) - SOLE PACKAGE MANAGER
- Configuration: `.config/home-manager/home.nix`
- Apply changes: `home-manager switch`
- **Manages 95+ packages**: ALL development tools, desktop applications, Wayland environment, system libraries
- Integrated configuration management for git, zsh, emacs, firefox
- Allows unfree packages (claude-code, spotify, steam)
- **Complete package management solution - use for ALL package installations**

### Guix (ELIMINATED)
- **Status**: Completely eliminated as of 2025-07-27
- Package manifest: `.config/guix/packages.scm` (empty - `(specifications->manifest (list))`)
- **Migration Complete**: All packages moved to home-manager/Nix
- **Note**: Guix daemon may still be present but no packages are managed through it

## Key Configuration Files

### Window Manager & Desktop
- **Sway config**: `.config/sway/config` - Wayland compositor configuration
- **Waybar**: Window manager bar configuration
- **Wofi**: Application launcher (dmenu replacement)
- **Swaylock**: Screen lock with U2F authentication support

### Development Environment
- **Emacs**: `.config/emacs/init.el` - Main editor configuration
  - Uses XDG directories for cache/data/state
  - Package management through MELPA/GNU ELPA
- **Terminal**: Alacritty configured as default terminal
- **Shell**: Zsh with custom configuration

## Common Development Commands

### System Management
```bash
# Apply home-manager configuration (primary package management)
home-manager switch

# System package updates (DNF for base system only)
sudo dnf update

# Reload Sway configuration
swaymsg reload
```

### Package Management
```bash
# ONLY package management method (since 2025-07-27)
# Edit .config/home-manager/home.nix, then:
home-manager switch

# Legacy Guix commands (NO LONGER USED)
# All packages migrated to home-manager - Guix eliminated
```

## Authentication & Security

### U2F Hardware Keys
- Configured for GDM, sudo, and swaylock
- Key mappings stored in `/etc/u2f_mappings`
- Supports multiple hardware keys
- Used with GitHub, GitLab, Porkbun, Namecheap, and Bitwarden

## Architecture Notes

### XDG Directory Compliance
- Emacs configured to use XDG directories instead of cluttering home
- Cache: `$XDG_CACHE_HOME/emacs/`
- Data: `$XDG_DATA_HOME/emacs/`
- State: `$XDG_STATE_HOME/emacs/`

### Wayland-First Environment
- Sway as the main compositor
- Firefox and other applications configured for Wayland
- Uses waybar, wofi, mako for desktop environment components

### Known Issues & Workarounds
- Coming back from sleep doesn't restore laptop screen
- Firefox-wayland crashes when reloading sway/returning from sleep
- Waybar occasionally disappears (restart with `nohup waybar &`)
- Some applications don't respect XDG directories (Firefox, Thunderbird, Kodi)

## Architecture Decisions & Process

### ADR Management
- **Location**: All Architecture Decision Records are stored in `docs/architecture/adr/`
- **Current ADRs**: See `docs/architecture/adr/README.md` for complete index
- **Example**: [ADR-001: Package Manager Consolidation](docs/architecture/adr/0001-package-manager-consolidation.md)

### For Future Claude Code Sessions

#### Planning & Decision Making
- **ALWAYS create new ADRs** for significant architectural changes during planning phases
- **Use TodoWrite tool** to track implementation progress for complex changes
- **Supersede existing ADRs** if requirements change or better approaches are identified
- **Reference existing ADRs** when making related decisions

#### Git Workflow
- **Commit after each milestone** - don't batch commits across major implementation phases
- **Update CLAUDE.md** if user requests deviate from documented architecture
- **Include ADR updates** in commits when architectural decisions change
- **Use descriptive commit messages** that reference ADR numbers when applicable

#### When to Update CLAUDE.md
- User requests significantly different approach than documented
- New recurring patterns emerge that future sessions should know
- Major architectural changes that affect session prompts
- Package management or tooling changes that impact workflow

#### Commit Message Format
Follow imperative mood (complete: "If applied, this commit will ___"):
```bash
git commit -m "Add feature X to improve Y

- Reference ADR-XXX if applicable  
- Include rationale for architectural changes
- Explain why change was needed, not how

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

**Rules:**
- Start with imperative verb (Add, Fix, Update, Remove, Refactor)
- Limit subject to 50 characters, capitalize, no period
- Body explains "what" and "why", not "how"
- Reference ADRs when making architectural decisions

### Package Management Hierarchy (Current)
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

## File Monitoring

The `.gitignore` file specifies which configuration files are tracked:
- Most of `.local/` is ignored except `.local/bin/pavolume`
- All of `.config/` is tracked
- Root-level configuration files like `.gitconfig` are tracked
