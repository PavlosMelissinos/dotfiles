# greetd + nwg-hello Implementation Plan - 2025

**Date**: 2025-08-12
**Status**: Ready for Implementation
**Priority**: High
**Target**: Replace unused GDM with lightweight greetd + nwg-hello

## Current Resource Waste Analysis

### GDM Process Audit
```bash
Total GDM-related memory usage: ~222MB
Key wasteful processes:
- gnome-shell: ~94MB
- Various gsd-* daemons: ~50MB
- Xwayland session: ~15MB
- GNOME session infrastructure: ~60MB
```

**Problem**: You're running a full GNOME Shell greeter session on TTY1 while
manually starting Sway on TTY2, wasting ~222MB RAM and CPU cycles.

**Solution**: Replace with greetd (minimal daemon) + nwg-hello (lightweight
greeter) for estimated savings of ~200MB.

---

## Implementation Strategy

### Phase 1: System Preparation (30 minutes)

#### 1.1 Install Required Packages
Since you're on Fedora (not NixOS), greetd needs manual system installation:

```bash
# Install greetd from repositories or build from source
# Note: greetd may not be in Fedora repos, check availability
sudo dnf search greetd
# If not available, will need to compile from source
```

#### 1.2 Home-Manager Package Addition
```nix
# Add to .config/home-manager/home.nix
home.packages = with pkgs; [
  nwg-hello
  cage          # Wayland compositor for the greeter
];
```

### Phase 2: greetd Configuration (1-2 hours)

#### 2.1 Create greetd Configuration
```toml
# /etc/greetd/config.toml
[terminal]
vt = 1

[default_session]
command = "cage -s -- nwg-hello"
user = "greeter"

[initial_session]
command = "sway"
user = "pavlos"
```

#### 2.2 Create Greeter User
```bash
# Create system greeter user
sudo useradd -r -s /bin/false greeter
sudo usermod -aG video greeter
```

#### 2.3 PAM Configuration for U2F
```bash
# Ensure U2F works with greetd
# /etc/pam.d/greetd should include your existing U2F configuration
sudo cp /etc/pam.d/gdm-password /etc/pam.d/greetd
# Verify U2F lines are present
```

### Phase 3: nwg-hello Configuration (30 minutes)

#### 3.1 Create nwg-hello Configuration
```nix
# Add to home.nix for user configuration
xdg.configFile."nwg-hello/nwg-hello-default.json".text = builtins.toJSON {
  "cmd_lock" = "swaylock -f";
  "cmd_sleep" = "systemctl suspend";
  "cmd_reboot" = "systemctl reboot";
  "cmd_poweroff" = "systemctl poweroff";
  "autologin_user" = "";
  "autologin_session" = "sway";
  "gtk_theme" = "Adwaita";
  "gtk_icon_theme" = "Adwaita";
  "prefer_dark_theme" = true;
  "lang" = "en_US";
  "keyboard_layout" = "us";
  "custom_sessions" = [
    {
      "name" = "Sway";
      "exec" = "sway";
    }
  ];
};

# CSS styling for dark theme
xdg.configFile."nwg-hello/style.css".text = ''
  #window {
    background-color: #1e1e2e;
    color: #cdd6f4;
    font-family: "JetBrains Mono", monospace;
  }

  #user-input, #password-input {
    border-radius: 8px;
    background-color: #313244;
    color: #cdd6f4;
    border: 1px solid #45475a;
    padding: 8px;
    margin: 4px;
  }

  #login-button {
    background-color: #89b4fa;
    color: #1e1e2e;
    border-radius: 8px;
    padding: 8px 16px;
    border: none;
    font-weight: bold;
  }

  #login-button:hover {
    background-color: #74c7ec;
  }
'';
```

### Phase 4: System Service Migration (1 hour)

#### 4.1 Service Replacement
```bash
# Stop and disable GDM
sudo systemctl stop gdm.service
sudo systemctl disable gdm.service

# Enable greetd
sudo systemctl enable greetd.service

# Test configuration before reboot
sudo greetd --config /etc/greetd/config.toml --check
```

#### 4.2 Backup and Rollback Plan
```bash
# Create system restore point
sudo systemctl set-default multi-user.target  # Fallback to CLI login
sudo cp /etc/greetd/config.toml /etc/greetd/config.toml.backup

# Rollback procedure if needed:
# sudo systemctl disable greetd.service
# sudo systemctl enable gdm.service
# sudo systemctl set-default graphical.target
```

---

## Expected Benefits

### Resource Savings
```
┌─ RESOURCE COMPARISON ───────────────────────────────────────┐
│ Component       │ Current (GDM)  │ After (greetd)  │ Savings │
│ Memory Usage    │ ~222MB         │ ~20MB           │ ~200MB  │
│ CPU (idle)      │ ~2-5%          │ ~0.1%           │ ~95%    │
│ Process Count   │ ~25 processes  │ ~3 processes    │ ~22     │
│ Dependencies    │ GNOME stack    │ Minimal         │ Huge    │
└─────────────────────────────────────────────────────────────┘
```

### Functional Improvements
- **Wayland-native**: No X11/Xwayland in login flow
- **Faster boot**: Eliminates GNOME Shell startup delay
- **Customizable**: CSS styling, multi-monitor config
- **Lightweight**: Minimal resource footprint
- **Sway-optimized**: Perfect integration with your workflow

---

## Implementation Timeline

### Recommended Schedule
```
Day 1 (Evening - 2 hours):
├─ Install packages via home-manager
├─ Research greetd Fedora installation
├─ Create configuration files
└─ Test nwg-hello manually

Day 2 (Weekend - 3 hours):
├─ Install greetd system service
├─ Configure PAM and U2F integration
├─ Test login functionality
├─ Switch services and reboot test
└─ Validate complete functionality
```

### Risk Mitigation
1. **Keep GDM available** for rollback
2. **Test extensively** before final switch
3. **Have TTY access** for recovery
4. **Backup configurations** before changes

---

## Validation Checklist

### Pre-Migration Tests
- [ ] nwg-hello runs manually from terminal
- [ ] greetd config syntax is valid
- [ ] PAM configuration includes U2F
- [ ] Greeter user has proper permissions

### Post-Migration Tests
- [ ] System boots to greetd greeter
- [ ] U2F hardware key authentication works
- [ ] Password authentication works
- [ ] Sway session launches correctly
- [ ] Multi-monitor configuration preserved
- [ ] Sleep/resume functionality intact
- [ ] Power management (shutdown/reboot) works

### Success Metrics
- [ ] Memory usage reduced by ~200MB
- [ ] Boot time improved
- [ ] No GNOME processes running
- [ ] Login experience feels faster
- [ ] All existing functionality preserved

---

## Troubleshooting Guide

### Common Issues

#### 1. greetd Service Fails to Start
```bash
# Check logs
sudo journalctl -u greetd.service -f

# Common causes:
# - Invalid config.toml syntax
# - Missing greeter user
# - Incorrect file permissions
```

#### 2. nwg-hello Doesn't Display
```bash
# Test manually
cage -s -- nwg-hello

# Check for:
# - Missing dependencies
# - Wayland display issues
# - CSS syntax errors
```

#### 3. U2F Authentication Fails
```bash
# Verify PAM configuration
sudo cat /etc/pam.d/greetd

# Ensure includes:
# auth required pam_u2f.so
```

#### 4. Session Launch Issues
```bash
# Test Sway launch manually
greetd-greet --cmd "sway"

# Check for:
# - Environment variables
# - Session script issues
# - Wayland socket problems
```

---

## Alternative Approaches

### Option 1: Minimal Implementation
If greetd installation proves complex:
```toml
# Ultra-minimal config with autologin
[default_session]
command = "sway"
user = "pavlos"
```

### Option 2: Text-Based Alternative
Use tuigreet for simplicity:
```toml
[default_session]
command = "tuigreet --cmd sway"
user = "greeter"
```

### Option 3: No Display Manager
Completely eliminate display manager:
```bash
# Configure TTY autologin
sudo systemctl edit getty@tty1.service
# Add autologin configuration
```

---

## Expected Outcome

**Goal**: Eliminate 222MB of unnecessary GNOME processes while maintaining all
functionality.

**Result**: Clean, lightweight, Wayland-native login experience perfectly suited
to your Sway environment.

**Success Criteria**:
- Login works seamlessly with U2F
- Resource usage drops significantly
- Boot time improves
- No functionality lost
- Customizable, modern interface

This implementation will transform your system from "GDM + manual Sway" to
"greetd + automated Sway launch" with substantial resource savings and better
architectural alignment.

---

**Status**: Ready for Implementation
**Next Steps**: Begin with Phase 1 package installation and configuration file creation
**Estimated Completion**: 2 evenings of focused work with significant system improvements
