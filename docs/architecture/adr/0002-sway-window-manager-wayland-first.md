# ADR-0002: Sway Window Manager with Wayland-First Desktop Environment

**Date**: 2025-07-27
**Status**: Accepted

## Context

The Linux desktop environment requires a window manager and
compositor. Traditional options include:
- **X11-based**: i3, Awesome, Xmonad, GNOME/X11, KDE/X11
- **Wayland-based**: Sway, GNOME/Wayland, KDE/Wayland, Hyprland

Key factors influencing this decision:
- Modern graphics stack requirements (better security, performance)
- Future-proofing as X11 is being deprecated
- Tiling window manager preference for development workflow
- Hardware security features (screen capture restrictions)
- Multi-monitor support requirements
- Compatibility with existing i3 configurations

## Decision

We will use Sway as the primary window manager with a Wayland-first desktop
environment.

Supporting components:
- **Waybar**: Status bar replacement for i3bar
- **Wofi**: Application launcher (dmenu replacement)
- **Mako**: Notification daemon
- **Swaylock**: Screen locker with U2F support
- **Kanshi**: Multi-monitor configuration management
- **Gammastep**: Blue light filter (redshift replacement)

We will configure applications to prefer Wayland when available, falling back to
XWayland when necessary.

## Consequences

**Positive:**
- Modern graphics stack with better security and performance
- Future-proof architecture as industry moves away from X11
- Native multi-monitor support without xrandr complexity
- Better integration with modern applications
- Maintained compatibility with i3 configuration syntax
- Hardware security features (secure screen capture, input isolation)

**Negative:**
- Some legacy applications may have compatibility issues with XWayland
- Screen sharing in some applications requires specific Wayland protocols
- Debugging tools for Wayland are less mature than X11
- Some niche applications may not support Wayland properly
- Firefox-wayland occasionally crashes when reloading sway/returning from sleep
- Remote desktop solutions may require additional configuration
