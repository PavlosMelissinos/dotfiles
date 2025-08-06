# ADR-0009: Complete Guix Package Manager Elimination

**Date**: 2025-08-06  
**Status**: Accepted  
**Supersedes**: ADR-0001 (Package Manager Consolidation)  

## Context

Following ADR-0001's decision to consolidate package management, Guix was partially eliminated with its manifest emptied on 2025-07-27. However, 74 packages remained installed in the user profile despite the empty manifest, creating ongoing issues:

- **Environment Variable Contamination**: Guix paths remained in SSL_CERT_FILE, PATH, and other variables
- **Daemon Connection Issues**: `guix package` commands failed with "Connection refused" to daemon socket
- **Disk Space Usage**: ~4.2GB consumed by `/gnu/store/` and profile generations
- **Package Management Complexity**: Mixed environment with home-manager and lingering Guix packages
- **SSL Certificate Conflicts**: Applications failing due to non-existent Guix certificate paths

The partial elimination approach proved insufficient for achieving clean package management.

## Decision

**Complete elimination of Guix package manager from the system**, including:

1. **Package Removal**: Remove all 74 remaining Guix packages via `guix package --delete-generations`
2. **Store Cleanup**: Delete entire `/gnu/store/` and `/var/guix/` system directories
3. **Environment Cleanup**: Remove Guix profile sourcing from shell configuration (`.zshrc`)
4. **Symlink Removal**: Delete broken `~/.guix-profile` and `~/.config/guix/current` symlinks
5. **Directory Cleanup**: Remove all user Guix directories (`~/.config/guix/`, `~/.cache/guix`)
6. **Daemon Deactivation**: Stop and permanently disable `guix-daemon` systemd service

## Consequences

### Positive
- **Clean Environment**: No more Guix environment variable contamination
- **Simplified Package Management**: Single source of truth (home-manager/Nix)
- **Disk Space Recovery**: ~4.2GB freed from system directories
- **No SSL Certificate Conflicts**: Applications use proper system certificates
- **Reduced Complexity**: Eliminates mixed package management issues
- **Improved Reliability**: No more daemon connection failures

### Negative
- **No Rollback Path**: Complete removal means no easy path back to Guix
- **Package Migration Required**: All 74 Guix packages needed equivalent home-manager packages
- **Potential Missing Packages**: Some specialized Guix packages might not exist in Nixpkgs

### Neutral
- **Configuration Maintenance**: Simpler ongoing configuration management
- **System Integration**: Better integration with Wayland/Sway desktop environment

## Implementation

The complete elimination was performed following this sequence:

1. Remove Guix environment sourcing from `.config/home-manager/.zshrc`
2. Delete user symlinks and directories (no sudo required)
3. Temporarily start Guix daemon (`sudo systemctl start guix-daemon`)
4. Remove all packages (`guix package --delete-generations`)
5. Clean store (`guix gc --delete-generations`)
6. Stop and disable daemon permanently
7. Remove system directories (`sudo rm -rf /gnu/store/ /var/guix/`)
8. Update documentation to reflect complete elimination

## Verification

Success verified by:
- `guix` command no longer found
- No Guix environment variables in fresh shells
- System directories `/gnu/store/` and `/var/guix/` removed
- Home-manager packages work without SSL certificate errors
- Clean package management hierarchy achieved

This ADR supersedes ADR-0001 by completing the package manager consolidation with total Guix elimination rather than partial removal.