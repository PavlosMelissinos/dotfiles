# greetd Configuration Templates - 2025

**Date**: 2025-08-12  
**Status**: Ready for System Configuration  

## greetd Configuration Options

Since cage has EGL issues in your environment, here are the configuration options for `/etc/greetd/config.toml`:

### Option 1: nwg-hello with cage (if cage gets fixed)
```toml
[terminal]
vt = 1

[default_session]
command = "cage -s -- nwg-hello"
user = "greeter"

[initial_session]
command = "sway"
user = "pavlos"
```

### Option 2: wlgreet (Recommended for your setup)
```toml
[terminal]  
vt = 1

[default_session]
command = "wlgreet --command sway"
user = "greeter"

# Optional: Auto-login for your user (skips greeter entirely)
# [initial_session]
# command = "sway"
# user = "pavlos"
```

### Option 3: Direct Sway launch (minimal approach)
```toml
[terminal]
vt = 1

[default_session]
command = "sway"
user = "pavlos"
```

## System Setup Commands

### 1. Create greeter user
```bash
sudo useradd -r -s /bin/false greeter
sudo usermod -aG video greeter
```

### 2. Create greetd configuration
```bash
sudo mkdir -p /etc/greetd
sudo tee /etc/greetd/config.toml << 'EOF'
[terminal]
vt = 1

[default_session]
command = "wlgreet --command sway"
user = "greeter"
EOF
```

### 3. Configure PAM for U2F support
```bash
# Copy existing GDM PAM config as base
sudo cp /etc/pam.d/gdm-password /etc/pam.d/greetd

# Verify U2F configuration is included
sudo grep -n pam_u2f /etc/pam.d/greetd
```

### 4. Switch from GDM to greetd
```bash
# Stop GDM
sudo systemctl stop gdm.service
sudo systemctl disable gdm.service

# Enable greetd
sudo systemctl enable greetd.service

# Test before rebooting
sudo systemctl start greetd.service
```

### 5. Rollback procedure (if needed)
```bash
sudo systemctl stop greetd.service
sudo systemctl disable greetd.service
sudo systemctl enable gdm.service
sudo systemctl start gdm.service
```

## Validation Tests

### Test 1: greetd service syntax
```bash
sudo greetd --config /etc/greetd/config.toml --check
```

### Test 2: wlgreet manual test (if possible)
```bash
# This would normally be run by greetd, but we can test basic functionality
wlgreet --help
```

### Test 3: U2F PAM configuration
```bash
# Check PAM files include U2F
grep -r pam_u2f /etc/pam.d/
```

## Expected Resource Savings

Current GDM processes (~222MB):
- gnome-shell: ~94MB
- gsd-* daemons: ~50MB
- GNOME infrastructure: ~78MB

After greetd migration (~15MB):
- greetd daemon: ~5MB
- wlgreet: ~10MB
- Total savings: ~207MB

## Next Steps Recommendation

Given the cage EGL issue, I recommend:

1. **Use wlgreet approach** (Option 2 above)
2. **Test with initial_session** to auto-login to Sway
3. **Keep nwg-hello available** for future use if cage issues are resolved

The wlgreet approach will give you:
- Clean Wayland-native login
- Minimal resource usage  
- No compositor compatibility issues
- Easy rollback to GDM if needed

---

**Ready for Implementation**: Yes, proceed with wlgreet configuration