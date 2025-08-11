# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

## Project Overview

This is a personal dotfiles repository for a Linux desktop environment using
Sway (Wayland compositor), managed through home-manager (Nix) and traditional
configuration files. The system is configured for development work with Emacs,
terminal applications, and various GUI tools.

## Package Management & Configuration

### home-manager (Nix Flakes) - SOLE PACKAGE MANAGER
- **Configuration**: Flake-based setup in `.config/home-manager/`
  - `flake.nix` - Inputs and configuration structure
  - `home.nix` - Package and service declarations  
  - `flake.lock` - Pinned package versions for reproducibility
- **Apply changes**: `home-manager switch --flake .` (from `.config/home-manager/`)
- **Manages 95+ packages**: ALL development tools, desktop applications,
  Wayland environment, system libraries
- **nixGL integration** for hardware acceleration in GUI applications
- Integrated configuration management for git, zsh, emacs, firefox
- Allows unfree packages (claude-code, spotify, steam, viber)
- **Complete package management solution - use for ALL package installations**

### Guix (COMPLETELY ELIMINATED)
- **Status**: Fully removed as of 2025-08-06
- All packages, profiles, and system directories removed
- Environment variables cleaned from shell configuration
- Daemon stopped and disabled permanently
- **Result**: Clean home-manager-only package management achieved

## Key Configuration Files

### Window Manager & Desktop
- **Sway config**: `.config/sway/config` - Wayland compositor configuration
- **Waybar**: Window manager bar configuration
- **Wofi**: Application launcher (dmenu replacement)
- **Swaylock**: Screen lock (DISABLED - unlock issues with Nix version)

### Development Environment
- **Emacs**: `.config/emacs/init.el` - Main editor configuration
  - Uses XDG directories for cache/data/state
  - Package management through MELPA/GNU ELPA
- **Terminal**: Alacritty configured as default terminal
- **Shell**: Zsh with custom configuration

## Common Development Commands

### System Management
```bash
# Apply home-manager flake configuration (primary package management)
cd ~/.config/home-manager && home-manager switch --flake .

# System package updates (DNF for base system only)
sudo dnf update

# Reload Sway configuration
swaymsg reload

# Update flake inputs (like package updates in traditional package managers)
cd ~/.config/home-manager && nix flake update
```

### Package Management (Flakes)
```bash
# Modern flake-based workflow (since 2025-08-11)
cd ~/.config/home-manager

# 1. Edit home.nix to add/remove packages
# 2. Apply configuration
home-manager switch --flake .

# 3. Update packages (updates flake.lock)
nix flake update

# 4. Pin specific inputs if needed  
nix flake lock --update-input nixpkgs
```

## Authentication & Security

### U2F Hardware Keys
- Configured for GDM and sudo (swaylock disabled due to unlock issues)
- Key mappings stored in `/etc/u2f_mappings`
- Supports multiple hardware keys
- Used with GitHub, GitLab, Porkbun, Namecheap, and Bitwarden

## Hardware Acceleration & Graphics

### nixGL Integration
- **Purpose**: Provides OpenGL context for GUI applications in Nix
- **Configuration**: Integrated via flake input in `flake.nix`
- **Wrapper usage**: `config.lib.nixGL.wrap` for applications needing hardware acceleration
- **Default wrapper**: Mesa (Intel integrated graphics)

### Application-Specific Solutions

#### Viber (Messaging)
- **Approach**: nixGL-wrapped AppImage with optimized configuration
- **Font rendering**: Direct AppImage execution (bypasses font isolation issues)
- **Link handling**: Custom xdg-open wrapper with clean Firefox environment
- **Hardware acceleration**: nixGL provides proper OpenGL context
- **User experience**: No prompts, proper fonts, working links
- **Fallback**: appimage-run if direct execution fails
- **Location**: Managed declaratively in home.nix packages

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
- **ALWAYS commit changes** - This is mandatory after completing any implementation work
- **Commit after each milestone** - don't batch commits across major implementation phases
- **Update CLAUDE.md** if user requests deviate from documented architecture
- **Include ADR updates** in commits when architectural decisions change
- **Use descriptive commit messages** that reference ADR numbers when applicable
- **Standard commit workflow**:
  1. Run `git status` and `git diff` to review changes
  2. Stage files with `git add <files>`
  3. Commit with proper message format (see commit message standards below)
  4. Verify with `git status` to ensure clean working tree

#### When to Update CLAUDE.md
- User requests significantly different approach than documented
- New recurring patterns emerge that future sessions should know
- Major architectural changes that affect session prompts
- Package management or tooling changes that impact workflow

#### Claude Code Sudo Command Execution Issues
**Problem**: Claude Code cannot execute sudo commands due to password/authentication requirements
- **Error Pattern**: Commands requiring sudo fail when executed directly via Bash tool
- **Root Cause**: Interactive password prompts cannot be handled in non-interactive sessions
- **User Feedback**: "Claude has issues running commands that require superuser privileges"

**Required Workflow**:
1. **Identify sudo commands needed** (e.g., `sudo dnf remove viber`, `sudo mount -o remount`)
2. **Tell user which commands to run** - provide exact command syntax
3. **Wait for user confirmation** that command completed successfully
4. **Verify completion** using non-sudo commands before proceeding
5. **Never attempt direct sudo execution** via Bash tool

**Commands That Require This Workflow**:
- Package management: `sudo dnf install/remove/update`
- System configuration: `sudo mount`, `sudo systemctl --system`
- File operations in system directories: `sudo cp`, `sudo mv`, `sudo rm`
- Network/hardware configuration requiring root privileges

**Note for Future Sessions**: Always ask user to execute sudo commands manually rather than attempting direct execution

#### Code Quality Standards
- **Line Length**: Follow [ADR-0005](docs/architecture/adr/0005-line-length-text-formatting-standards.md) - limit lines to 80 characters
- **Trailing Whitespace**: Follow [ADR-0006](docs/architecture/adr/0006-trailing-whitespace-policy.md) - remove all trailing whitespace
- **Git Commits**: Follow [ADR-0007](docs/architecture/adr/0007-git-commit-message-standards.md) - use imperative mood, 50/72 rule

#### Commit Message Format
Follow imperative mood with tool/component prefix:
```bash
git commit -m "[tool] Add feature X to improve Y

- Reference ADR-XXX if applicable
- Include rationale for architectural changes
- Explain why change was needed, not how

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

**Rules:**
- **REQUIRED PREFIX**: Start with `[tool]` where tool is the primary component
  (e.g. `[home-manager]`, `[sway]`, `[waybar]`, `[docs]`, `[git]`)
- Use imperative verb after prefix (Add, Fix, Update, Remove, Refactor)
- Limit subject to 50 characters, capitalize, no period
- Body explains "what" and "why", not "how"
- Reference ADRs when making architectural decisions
- Follow [ADR-0007](docs/architecture/adr/0007-git-commit-message-standards.md)
  for complete standards

**Common Prefixes:**
- `[home-manager]` - Package management, home.nix changes
- `[sway]` - Window manager configuration
- `[docs]` - Documentation updates (CLAUDE.md, README, etc.)
- `[git]` - Git configuration and workflow changes

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
│  Flatpak (ELIMINATED - 2025-07-27)              │
│  └─ No packages remaining                       │
└─────────────────────────────────────────────────┘
```

## File Monitoring

The `.gitignore` file specifies which configuration files are tracked:
- Most of `.local/` is ignored except `.local/bin/pavolume`
- All of `.config/` is tracked
- Root-level configuration files like `.gitconfig` are tracked

## Collaboration Guidelines

- **Challenge and question**: Don't immediately agree or proceed with requests
  that seem suboptimal, unclear, or potentially problematic
- **Push back constructively**: If a proposed approach has issues, suggest
  better alternatives with clear reasoning
- **Think critically**: Consider edge cases, performance implications,
  maintainability, and best practices before implementing
- **Seek clarification**: Ask follow-up questions when requirements are
  ambiguous or could be interpreted multiple ways
- **Propose improvements**: Suggest better patterns, more robust solutions, or
  cleaner implementations when appropriate
- **Be a thoughtful collaborator**: Act as a good teammate who helps improve the
  overall quality and direction of the project
