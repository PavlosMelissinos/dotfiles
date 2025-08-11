# ADR-0010: Home Manager Flakes Migration

**Date**: 2025-08-11
**Status**: Accepted

## Context

Home Manager was initially configured using the traditional channels-based
approach, which had several limitations:

- **Reproducibility Issues**: Channel updates could change package versions
  unpredictably
- **Dependency Management**: No explicit dependency graph for inputs like nixGL
- **Version Pinning**: Difficult to pin specific versions of inputs
- **Modern Workflow**: Flakes represent the modern, preferred approach in the
  Nix ecosystem
- **Integration Complexity**: Adding external dependencies (like nixGL) was
  cumbersome

The channels approach was functional but lacked the precision and
reproducibility needed for a stable desktop environment configuration.

## Decision

We will migrate from channels-based to flakes-based Home Manager configuration.

### Implementation Strategy
1. **Create flake.nix**: Define inputs (nixpkgs, home-manager, nixGL) with
   version pinning
2. **Update home.nix**: Make compatible with flakes by accepting the new
   argument structure
3. **Enable flakes**: Configure `nix.conf` with experimental features
4. **Update workflow**: Change from `home-manager switch` to `home-manager
   switch --flake .`
5. **Dependency management**: Manage all external dependencies through flake
   inputs

### Architecture
```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixgl = {
      url = "github:nix-community/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
}
```

## Consequences

### Positive
- ✅ **Reproducible Builds**: `flake.lock` pins exact versions of all inputs
- ✅ **Modern Workflow**: Aligned with current Nix best practices
- ✅ **Explicit Dependencies**: All inputs clearly defined in `flake.nix`
- ✅ **Easy Updates**: `nix flake update` to update all inputs
- ✅ **Selective Updates**: `nix flake lock --update-input nixpkgs` for specific
  inputs
- ✅ **nixGL Integration**: Seamless hardware acceleration support
- ✅ **Version Control**: `flake.lock` tracks exact dependency versions

### Negative
- ⚠️ **Learning Curve**: Requires understanding flakes concepts and syntax
- ⚠️ **Command Changes**: Must use `--flake .` flag for all home-manager commands
- ⚠️ **Experimental Feature**: Requires enabling experimental flakes support
- ⚠️ **Migration Complexity**: One-time migration effort required

### Neutral
- **File Structure**: Adds `flake.nix` and `flake.lock` files to configuration
  directory
- **Configuration Location**: No change to existing `home.nix` location

## Implementation Details

### Files Created
- **flake.nix**: Main flake configuration with inputs and outputs
- **flake.lock**: Automatically generated lockfile with pinned versions

### Configuration Changes
- **nix.conf**: Added `experimental-features = nix-command flakes`
- **home.nix**: Updated to accept `nixgl` parameter for hardware acceleration
- **Commands**: Updated from `home-manager switch` to `home-manager switch
  --flake .`

### Workflow Updates
```bash
# New flake-based workflow
cd ~/.config/home-manager

# Apply configuration
home-manager switch --flake .

# Update all inputs
nix flake update

# Update specific input
nix flake lock --update-input nixpkgs
```

## Verification Results

- ✅ Flakes-based configuration successfully applied
- ✅ nixGL integration working for hardware acceleration
- ✅ Reproducible builds with locked dependencies
- ✅ All packages and services functioning correctly
- ✅ Modern flake commands operational

This migration establishes a modern, reproducible foundation for ongoing Home
Manager configuration management.
