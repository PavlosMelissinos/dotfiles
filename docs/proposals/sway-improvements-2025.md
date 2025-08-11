# Sway Configuration Improvements Proposal - 2025

**Date**: 2025-07-31
**Status**: Proposal
**Priority**: High (Functional Fixes), Medium (Visual Improvements)

## Executive Summary

This document proposes comprehensive improvements to the Sway window manager
configuration, addressing both critical workspace assignment issues and modern
visual enhancements available in 2025. The functional fixes are high priority
as they directly impact workflow, while visual improvements offer modernization
opportunities.

## Problem Analysis

### Current Workspace Assignment Issues

**Observed Behavior**: Applications are not being assigned to their configured
workspaces as defined in `.config/sway/config:195-198`.

**Root Cause Analysis**:
Using `swaymsg -t get_tree` revealed the actual application identifiers differ
from configuration assumptions:

| Application | Config Uses                                | Actual Identifier    | Status                |
|-------------|--------------------------------------------|----------------------|-----------------------|
| Alacritty   | `app_id="Alacritty" title=".*main-term.*"` | `app_id="Alacritty"` | Too restrictive       |
| Firefox     | `app_id="firefox" con_mark="main-firefox"` | `app_id="firefox"`   | Unnecessary condition |
| Emacs       | `app_id="emacs"`                           | `class="Emacs"`      | Wrong identifier type |
| Signal      | Missing rule                               | `app_id="signal"`    | No assignment         |
| Viber       | Missing rule                               | TBD when running     | No assignment         |

**Current Workspace Status**:
- Workspace 2: Alacritty (correct)
- Workspace 3: Firefox, Signal (Firefox correct, Signal unassigned)
- Workspace 4: Emacs (correct)

## Functional Fixes

### Priority 1: Workspace Assignment Corrections

**Target Configuration**:
- Alacritty → workspace 2
- Emacs → workspace 4
- Firefox → workspace 3
- Signal → workspace 3
- Viber → workspace 3

**Required Changes** in `.config/sway/config:195-198`:

```bash
# Current (broken)
for_window [app_id="Alacritty" title=".*main-term.*"] move to workspace 2
for_window [app_id="firefox" con_mark="main-firefox"] mark --add main-firefox, move to workspace 3
for_window [app_id="Kodi"] move to workspace 6
for_window [app_id="emacs"] move to workspace 4

# Proposed (fixed)
for_window [app_id="Alacritty"] move to workspace 2
for_window [app_id="firefox"] move to workspace 3
for_window [app_id="Kodi"] move to workspace 6
for_window [class="Emacs"] move to workspace 4
for_window [app_id="signal"] move to workspace 3
for_window [app_id="viber"] move to workspace 3  # Verify app_id when testing
```

### Diagnostic Commands

Before implementation:
```bash
# Verify current app identifiers
swaymsg -t get_tree | jq -r '.. | select(.app_id? or .window_properties?.class?) | "\(.app_id // .window_properties.class) - \(.name)"'

# Check workspace assignments
swaymsg -t get_workspaces
```

After implementation:
```bash
# Test workspace assignments after Sway reload
swaymsg reload
# Launch apps and verify placement
```

## Visual Improvements

### Current Visual Stack Analysis

**Installed Tools** (from home-manager):
- waybar: Status bar ✓
- kanshi: Display management ✓
- swaylock-effects: Enhanced lock screen (commented out)
- wob: Basic notification overlays

**Missing Popular 2025 Tools**:

#### Notification/Overlay Systems
| Tool          | Description                       | Advantages                                     |
|---------------|-----------------------------------|------------------------------------------------|
| SwayOSD       | GTK-based OSD with auto-detection | Modern, comprehensive, automatic key detection |
| avizo         | GNOME-like notification daemon    | Beautiful styling, praised aesthetics          |
| wob (current) | Lightweight overlay bar           | Basic, requires manual scripting               |

#### Application Launchers
| Tool           | Description                          | Status              |
|----------------|--------------------------------------|---------------------|
| wofi (current) | Wayland-native dmenu replacement     | Basic functionality |
| bemenu         | Native Wayland dmenu replacement     | More features       |
| tofi           | Dynamic menu for wlroots compositors | Modern, fast        |
| wmenu          | Sway-focused menu inspired by dmenu  | Lightweight         |

#### Screenshot Tools
| Tool                  | Description                         | Status         |
|-----------------------|-------------------------------------|----------------|
| grim+swappy (current) | Screenshot + annotation             | Functional     |
| wayshot               | Native Wayland screenshot tool      | More efficient |
| weye                  | Rust-based screenshot tool for Sway | Modern, fast   |

#### Additional Enhancement Options
| Tool          | Description                   | Use Case                  |
|---------------|-------------------------------|---------------------------|
| wlsunset      | Day/night gamma adjustment    | Eye strain reduction      |
| wdisplays     | GUI display configuration     | Alternative to kanshi CLI |
| ironbar       | GTK4 status bar with popups   | Alternative to waybar     |
| i3status-rust | Resource-efficient status bar | Alternative to waybar     |

### Advanced Visual Effects

**SwayFX Consideration**:
SwayFX is a popular fork offering advanced visual effects:
- Window blur and transparency
- Animations and transitions
- Enhanced visual effects
- Maintains i3 compatibility

**Trade-offs**:
- More resource intensive
- Additional complexity
- Potential stability considerations

## Implementation Priority Matrix

### High Priority (Immediate)
1. **Fix workspace assignments** - Critical functionality issue
2. **Enable swaylock-effects** - Already installed, just uncommented
3. **Add missing Signal/Viber rules** - Complete workspace management

### Medium Priority (Quality of Life)
1. **Replace wob with SwayOSD/avizo** - Better visual experience
2. **Add wlsunset** - Automatic gamma adjustment
3. **Consider wayshot** - Better screenshot performance

### Low Priority (Enhancement)
1. **Alternative launchers** - tofi/bemenu for variety
2. **Alternative status bars** - ironbar/i3status-rust exploration
3. **GUI display management** - wdisplays for easier configuration

### Optional (Advanced Users)
1. **SwayFX migration** - Advanced visual effects
2. **Custom waybar configuration** - Enhanced status bar styling
3. **Advanced notification theming** - Custom OSD styling

## Implementation Steps

### Phase 1: Critical Fixes
1. Backup current configuration
2. Update workspace assignment rules
3. Test with `swaymsg reload`
4. Verify app placement with diagnostic commands

### Phase 2: Visual Improvements
1. Add SwayOSD/avizo to home-manager packages
2. Update volume/brightness keybindings
3. Enable swaylock-effects
4. Add wlsunset for gamma management

### Phase 3: Optional Enhancements
1. Experiment with alternative tools
2. Consider SwayFX if advanced effects desired
3. Customize themes and styling

## Risk Assessment

**Low Risk**:
- Workspace assignment fixes (easily revertible)
- Adding new packages via home-manager

**Medium Risk**:
- SwayFX migration (significant change)
- Major keybinding modifications

**Mitigation**:
- Maintain configuration backups
- Test changes incrementally
- Use `swaymsg reload` rather than restart for testing

## Conclusion

The functional fixes should be implemented immediately to restore proper
workspace behavior. Visual improvements can be implemented incrementally based
on preference and available time. The 2025 Wayland ecosystem offers significant
improvements over basic configurations, particularly in notification systems
and modern tooling.

---

**Next Steps**: Review proposal, prioritize implementations, and execute in
phases with proper testing at each stage.
