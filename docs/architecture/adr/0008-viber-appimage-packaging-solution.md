# ADR-0008: Viber AppImage Packaging Solution

**Date**: 2025-07-28
**Status**: Accepted

## Context

The nixpkgs viber package is broken due to libxml2 compatibility issues (GitHub
Issue #421440), preventing installation through home-manager/Nix. This creates a
critical gap in messaging functionality and violates the unified package
management architecture established in ADR-0001.

### Problems Identified
- **Broken nixpkgs package**: Viber fails to build due to libxml2 dependency
  conflicts
- **Architecture violation**: User previously installed viber via DNF/RPM,
  breaking home-manager-only policy
- **Notification bypass**: System-level viber installation bypassed mako
  notification daemon
- **Missing functionality**: No working Viber messaging capability under unified
  package management

### Requirements
- Maintain home-manager/Nix as sole package manager
- Provide working Viber messaging functionality
- Ensure proper desktop notification integration with mako
- Follow XDG directory compliance where possible

## Decision

We will use the official Viber AppImage with home-manager integration support,
removing the system-level package and managing all dependencies through Nix.

### Implementation Strategy
1. **Remove system violation**: `sudo dnf remove viber`
2. **Add AppImage support**: Include `appimage-run` and OpenGL libraries in
   home-manager
3. **Create integration layer**: Launcher script and desktop file for proper
   integration
4. **Fix notification system**: Convert mako to systemd user service for D-Bus
   handling
5. **Maintain unified architecture**: All dependencies managed through home.nix

## Consequences

### Positive
- ✅ **Unified package management**: All viber dependencies managed through
  home-manager
- ✅ **Working functionality**: Reliable Viber messaging and calling
- ✅ **Proper notifications**: Integrates with mako notification system via
  D-Bus
- ✅ **XDG compliance**: Uses `.local/bin/` for user binaries and scripts
- ✅ **Maintainable solution**: Clear upgrade path via AppImage replacement
- ✅ **Architecture consistency**: Maintains home-manager-only policy

### Negative
- ⚠️ **Manual updates**: AppImage updates require manual intervention
- ⚠️ **Notification styling**: Uses Viber branding instead of system mako theme
- ⚠️ **Positioning issues**: Notifications appear center-screen vs configured
  position
- ⚠️ **Additional complexity**: Requires launcher script and environment
  configuration

### Known Issues
- Notification styling doesn't match system mako theme (purple Viber branding)
- Notifications appear in center of screen instead of configured mako position
- Functional but incomplete desktop notification integration

## Implementation Details

### Package Dependencies Added to home.nix
```nix
# AppImage support and graphics libraries
appimage-run
mesa
libGL
libglvnd
```

### Integration Files Created
- **Launcher script**: `.local/bin/viber` with Qt/OpenGL environment setup
- **Desktop file**: `.local/share/applications/viber-appimage.desktop`
- **AppImage binary**: `.local/bin/viber.AppImage` (official Viber release)

### Launcher Script Configuration
```bash
#!/bin/bash
export QT_QPA_PLATFORM=xcb
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export QT_SCALE_FACTOR=1
export QTWEBENGINE_DISABLE_SANDBOX=1
export LIBGL_ALWAYS_SOFTWARE=1
export QT_OPENGL=software
export QT_QUICK_BACKEND=software
exec appimage-run /home/pavlos/.local/bin/viber.AppImage "$@"
```

### Systemd Service Enhancement
```nix
# Mako notification daemon as systemd user service
systemd.user.services.mako = {
  Unit = {
    Description = "Mako notification daemon";
    PartOf = [ "graphical-session.target" ];
  };
  Service = {
    Type = "dbus";
    BusName = "org.freedesktop.Notifications";
    ExecStart = "${pkgs.mako}/bin/mako";
    Restart = "on-failure";
  };
};
```

### Verification Results
- ✅ Viber messaging and calling functional
- ✅ Notifications appear via mako (with styling limitations)
- ✅ All dependencies managed through home-manager
- ✅ No system-level package manager violations
- ✅ Desktop integration working (launcher, window management)

## References

- [GitHub Issue #421440: viber libxml2 compatibility](https://github.com/NixOS/nixpkgs/issues/421440)
- [Official Viber Linux AppImage](https://www.viber.com/download/)
- [ADR-0001: Package Manager Consolidation](0001-package-manager-consolidation.md)
- [ADR-0003: XDG Directory Compliance](0003-xdg-directory-compliance.md)
- Implementation commits: a0ffdcf, 67975e4
