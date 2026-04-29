---
title: "Stremio & Kodi Hardware Acceleration Fix"
date: 2025-08-13
status: In Progress
category: Media
---

# Stremio & Kodi Hardware Acceleration Issues - Investigation & Resolution Plan

## Problem Statement

Two media applications installed via home-manager are experiencing hardware acceleration issues:
1. **Stremio**: Launches but has no video playback
2. **Kodi**: Fails to start entirely

## Current State Analysis

### Environment Setup ⚠️ **UPDATED CONTEXT**
- **Hardware**: Intel HD 520 graphics (ThinkPad with Intel i5-6300U)
- **System**: **Fedora 42 Workstation** (NOT NixOS yet) with Wayland/Sway compositor
- **Package Management**: **Hybrid setup** - DNF (system) + home-manager (user packages)
- **Hardware Acceleration**: Fedora's native Mesa 25.1.4 + Intel iHD drivers + nixGL bridge
- **Session**: Wayland with Sway (manually configured on Fedora)

### Fedora Graphics Stack Status ✅
```bash
# Confirmed working Fedora drivers:
$ vainfo
vainfo: VA-API version: 1.22 (libva 2.22.0)
vainfo: Driver version: Intel iHD driver for Intel(R) Gen Graphics - 25.2.6

# Available packages:
mesa-dri-drivers-25.1.4, mesa-libGL-25.1.4, intel-mediasdk-23.2.2
libva-intel-driver-2.4.1, mesa-vulkan-drivers-25.1.4
```

### Stremio Issues
- **Status**: Currently running (processes visible: stremio + node server)
- **Root Cause**: nixGL wrapper cannot bridge to Fedora's graphics drivers
- **Current Installation**: Plain nix package without access to Fedora's VAAPI/Mesa
- **Configuration**: Has unfree package permissions but nixGL not bridging properly

### Kodi Issues  
- **Status**: Fails to start/crashes on startup
- **Root Cause**: nixGL wrapper cannot access Fedora's Intel graphics drivers
- **Sway Integration**: Assigned to workspace 6 but crashes before window creation
- **Hardware Dependencies**: Needs nixGL bridge to Fedora's Mesa + Intel iHD drivers

### Current Hardware Acceleration Setup
```nix
# Working configuration in home.nix:
nixGL.packages = nixgl.packages;
nixGL.defaultWrapper = "mesa";
nixGL.installScripts = [ "mesa" ];

# Successfully working example (Viber):
config.lib.nixGL.wrap (pkgs.writeShellScriptBin "viber" ''
  # Hardware acceleration provided by nixGL wrapper
'')
```

### System Graphics Configuration
```nix
# From configuration.nix - properly configured:
hardware.graphics = {
  enable = true;
  enable32Bit = true;
  extraPackages = with pkgs; [
    intel-media-driver  # VAAPI driver for newer Intel GPUs
    intel-vaapi-driver  # VAAPI driver for older Intel GPUs (HD 520)
    libvdpau-va-gl
  ];
};
```

## Root Cause Analysis ⚠️ **UPDATED FOR FEDORA**

The core issue is more complex on Fedora than NixOS: both Stremio and Kodi are installed as nix packages that cannot access Fedora's system graphics libraries, even with nixGL wrappers.

**Fedora-Specific Challenges:**
- **Driver Isolation**: Nix packages are isolated from Fedora's Mesa/Intel drivers
- **Library Path Mismatch**: nixGL must bridge between nix store and `/usr/lib64` system libraries  
- **VAAPI Access**: Fedora's Intel iHD drivers in `/usr/lib64/dri/` not accessible to nix packages
- **Home-Manager Integration**: No systemd service means manual activation required

**Why this is harder than NixOS:**
- NixOS: nixGL designed for nix-managed graphics stack
- Fedora: nixGL must bridge between nix packages and foreign system drivers
- Additional complexity: Wayland + Sway manually configured on Fedora

## Proposed Resolution Plan ⚠️ **UPDATED FOR FEDORA**

### Phase 1: Diagnose nixGL-Fedora Integration (Critical First Step)
**Objective**: Verify nixGL can access Fedora's graphics drivers

**Tasks:**
1. Test basic nixGL functionality: `nixGL glxinfo` or `nixGL mesa-demos`
2. Check if nixGL can access `/usr/lib64/dri/` Intel drivers
3. Verify VAAPI bridge: `nixGL vainfo` should show Intel iHD drivers
4. Test home-manager activation: `home-manager switch --flake .`

**Expected Result**: nixGL successfully bridges to Fedora's Mesa/Intel graphics stack

### Phase 2: Fix Home-Manager Integration
**Objective**: Ensure reliable home-manager operation on Fedora

**Tasks:**
1. Verify home-manager packages are properly linked after switch
2. Check if nixGL wrappers are actually being applied
3. Test if environment variables are correctly set
4. Consider setting up systemd service for automatic activation

**Expected Result**: home-manager switch reliably applies nixGL wrappers

### Phase 3: Fix Stremio (After nixGL verified working)
**Objective**: Enable video playback with hardware acceleration

**Tasks:**
1. Apply nixGL wrapper and test basic startup
2. Verify electron can access hardware acceleration through nixGL
3. Test video playback with various formats
4. Check if additional Wayland flags needed for electron apps

**Expected Result**: Stremio plays videos using Fedora's Intel hardware acceleration

### Phase 4: Fix Kodi (After Stremio working)
**Objective**: Enable Kodi startup and media functionality

**Tasks:**
1. Apply nixGL wrapper and test basic startup without crashing
2. Verify Kodi can access VAAPI through nixGL bridge
3. Test basic media playback functionality
4. Validate Sway workspace assignment works correctly

**Expected Result**: Kodi launches and plays media using hardware acceleration

### Phase 5: Systematic Hardware Acceleration Review ⚠️ **FEDORA-SPECIFIC**
**Objective**: Prevent similar issues with other applications on hybrid system

**Tasks:**
1. Audit other media-related applications (mpv, vlc) for nixGL wrapper needs
2. Test nixGL bridge functionality with different application types
3. Standardize nixGL wrapper pattern for Fedora + home-manager setup
4. Document hybrid system requirements for future hardware-accelerated apps

**Expected Result**: Consistent hardware acceleration across all media applications

### Phase 6: Testing & Validation ⚠️ **FEDORA CONTEXT**
**Objective**: Ensure robust solution on hybrid Fedora system

**Tasks:**
1. **Functional Testing**: Both applications start and play various media formats using Fedora drivers
2. **Performance Testing**: Verify hardware acceleration reduces CPU usage (Intel HD 520 optimization)
3. **Integration Testing**: Confirm Sway window management works correctly on Fedora
4. **Regression Testing**: Ensure other nixGL apps (like Viber) continue working
5. **Fedora Stability**: Verify solution survives Fedora system updates

**Expected Result**: Reliable media playback with optimal Intel HD 520 performance

## Implementation Strategy ⚠️ **UPDATED FOR FEDORA**

### Step 1: Verify nixGL-Fedora Bridge
**Before making any home.nix changes:**
```bash
# Test if nixGL can access Fedora drivers
nixGL glxinfo | grep -i intel
nixGL vainfo  # Should show Intel iHD driver

# Test basic OpenGL
nixGL mesa-demos
```

### Step 2: Required Changes to `home.nix`
**Current problematic configuration:**
```nix
home.packages = with pkgs; [
  # ... other packages ...
  stremio  # <- Missing nixGL wrapper  
  kodi     # <- Missing nixGL wrapper
];
```

**Proposed solution (IF nixGL bridge works):**
```nix
home.packages = with pkgs; [
  # ... other packages ...
  
  # Stremio with Fedora graphics bridge
  (config.lib.nixGL.wrap stremio)
  
  # Kodi with Fedora graphics bridge
  (config.lib.nixGL.wrap kodi)
];
```

### Step 3: Manual Testing Process
**Critical on Fedora:** Test each change manually
```bash
# Apply changes
cd ~/.config/home-manager
home-manager switch --flake .

# Test applications individually
stremio  # Check for video playback
kodi     # Check for startup
```

### Technical Implementation Details (Fedora-Specific)

1. **nixGL Bridge**: Must successfully access `/usr/lib64/dri/` drivers
2. **Library Paths**: Ensure nix packages can find Fedora's Mesa libraries
3. **VAAPI Access**: Bridge to Fedora's Intel iHD drivers (not nix-provided)
4. **Home-Manager Manual**: No systemd service - manual `switch` required
5. **Environment Variables**: May need additional `LIBVA_*` or `MESA_*` variables

### Risk Assessment & Mitigation ⚠️ **UPDATED FOR FEDORA**

**Low Risk Factors:**
- Well-established nixGL pattern already working for Viber on Fedora
- No changes to Fedora system-level graphics configuration needed
- Isolated changes to specific package declarations in home.nix
- Easy rollback path via `home-manager switch` to previous generation

**Medium Risk Factors (Fedora-Specific):**
- nixGL bridge complexity higher than on NixOS
- Potential conflicts between nix packages and Fedora system libraries
- Home-manager activation may require manual triggers

**Mitigation Strategies:**
- Test nixGL basic functionality BEFORE making home.nix changes
- Apply changes incrementally (Stremio first, then Kodi)
- Maintain backup of working home-manager configuration
- Validate that existing hardware-accelerated apps continue working
- Test on Fedora system updates to ensure compatibility
- Document any Fedora-specific issues for future hybrid system reference

## Success Criteria ⚠️ **UPDATED FOR FEDORA CONTEXT**

### Primary Objectives
- [ ] Stremio: Full video playback functionality restored using Fedora's Intel drivers
- [ ] Kodi: Application launches and basic media library works with hardware acceleration
- [ ] Both applications utilize Intel HD 520 hardware acceleration (measurable CPU reduction)
- [ ] Sway window management integration intact on Fedora

### Secondary Objectives (Fedora-Specific)
- [ ] nixGL-Fedora bridge pattern documented for future hybrid system reference
- [ ] Other media applications (mpv, vlc) audited for nixGL wrapper requirements
- [ ] Performance improvements measurable with Intel HD 520 optimization
- [ ] Solution survives Fedora system updates without breaking
- [ ] Template created for adding hardware-accelerated apps to hybrid Fedora+home-manager setup

## Next Steps

1. **Execute Implementation**: Apply nixGL wrappers to both applications
2. **Test Functionality**: Verify video playback and application startup
3. **Performance Validation**: Confirm hardware acceleration is working
4. **Documentation Update**: Record solution in home-manager configuration comments

## Recommended Execution Approach ⚠️ **UPDATED FOR FEDORA**

This plan should be executed by a **nix-home-manager-expert** subagent because:

- **Specialized Knowledge**: Deep understanding of home-manager package configuration on hybrid systems
- **nixGL Expertise**: Familiar with hardware acceleration bridging between nix and foreign systems
- **Fedora Graphics Integration**: Experience with Intel graphics drivers and Wayland on Fedora
- **Hybrid System Management**: Can handle Fedora + home-manager package interactions
- **Testing Capability**: Knows how to validate hardware acceleration on non-NixOS systems
- **Debugging Skills**: Can troubleshoot nixGL bridge issues between nix store and `/usr/lib64`

The solution leverages existing working patterns (Viber nixGL wrapper on Fedora) and maintains the declarative, reproducible nature of the nix configuration while solving practical hardware acceleration issues in a hybrid Fedora environment. The approach is more complex than pure NixOS but follows established patterns for foreign system integration.