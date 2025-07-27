# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

## Project Overview

This is a personal dotfiles repository for a Linux desktop environment using
Sway (Wayland compositor), managed through home-manager (Nix) and traditional
configuration files. The system is configured for development work with Emacs,
terminal applications, and various GUI tools.

## Package Management & Configuration

### home-manager (Nix)
- Configuration: `.config/home-manager/home.nix`
- Apply changes: `home-manager switch`
- Packages are managed declaratively through Nix
- Allows unfree packages (claude-code, spotify)

### Guix (Alternative/Experimental)
- Channel config: `.config/guix/channels.scm`
- Package manifest: `.config/guix/packages.scm`
- Apply channels: `guix pull -C ~/.config/guix/channels.scm`
- Install packages: `guix package -m ~/.config/guix/packages.scm`
- Upgrade daemon: `sudo -i guix pull && systemctl restart guix-daemon.service`

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
# Apply home-manager configuration
home-manager switch

# Restart Guix daemon (if using Guix)
sudo systemctl restart guix-daemon.service

# Reload Sway configuration
swaymsg reload
```

### Package Management
```bash
# Install packages via home-manager
# Edit .config/home-manager/home.nix, then:
home-manager switch

# Install packages via Guix
guix package -m ~/.config/guix/packages.scm
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

## File Monitoring

The `.gitignore` file specifies which configuration files are tracked:
- Most of `.local/` is ignored except `.local/bin/pavolume`
- All of `.config/` is tracked
- Root-level configuration files like `.gitconfig` are tracked
