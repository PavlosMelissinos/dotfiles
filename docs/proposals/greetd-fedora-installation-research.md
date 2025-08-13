# greetd Installation Research for Fedora - 2025

**Date**: 2025-08-12  
**Status**: Research Phase  
**Target**: Determine best approach for greetd installation on Fedora

## Commands to Run for Research

### 1. Check Fedora Repository Availability
```bash
# Search official Fedora repos
sudo dnf search greetd

# Check if available in RPM Fusion or COPR
sudo dnf copr search greetd

# Check Fedora package database online
# Visit: https://packages.fedoraproject.org/search?query=greetd
```

### 2. Alternative Installation Methods

#### Option A: RPM Fusion/COPR Repository
```bash
# If found in COPR, enable the repo:
# sudo dnf copr enable <username>/greetd
# sudo dnf install greetd
```

#### Option B: Manual Compilation from Source
```bash
# Dependencies needed for compilation:
sudo dnf install rust cargo git

# Clone and build greetd:
git clone https://git.sr.ht/~kennylevinsen/greetd
cd greetd
cargo build --release

# Manual installation:
sudo cp target/release/greetd /usr/local/bin/
sudo cp greetd.service /etc/systemd/system/
```

#### Option C: Alternative Approach - Use existing SDDM/LightDM with nwg-hello
```bash
# If greetd proves difficult, consider using SDDM instead:
sudo dnf install sddm
# Configure SDDM to use a minimal theme and launch nwg-hello
```

## Expected Results

### If greetd is in Fedora repos:
- **Best case**: Direct dnf installation
- **Estimated time**: 30 minutes setup

### If greetd needs compilation:
- **Source build required**: ~1-2 hours
- **Dependencies**: Rust toolchain
- **Complexity**: Medium-High

### Fallback options:
- **SDDM alternative**: Easier installation, more resource usage
- **LightDM alternative**: Middle ground option

## Next Steps After Research

1. **Document findings**: Note which installation method is available
2. **Prepare system**: Install any required dependencies
3. **Test installation**: Verify greetd can be installed successfully
4. **Create greetd config**: Set up the daemon configuration
5. **Integration testing**: Test with nwg-hello before switching from GDM

## Configuration Templates Ready

We already have:
- ✅ nwg-hello installed and configured via home-manager
- ✅ CSS styling for dark theme matching your setup
- ✅ Multi-monitor configuration
- ✅ cage compositor installed

Remaining tasks:
- [ ] Install greetd system daemon
- [ ] Create /etc/greetd/config.toml
- [ ] Set up PAM configuration for U2F
- [ ] Test complete login flow
- [ ] Switch from GDM to greetd

## Risk Assessment

**Low Risk Approaches:**
- greetd available in official repos
- Proven COPR repository

**Medium Risk Approaches:**
- Manual compilation from source
- Alternative display manager (SDDM)

**High Risk Approaches:**
- Building custom RPM packages
- Modifying system authentication significantly

## Success Criteria

After installation, we should be able to:
1. Start greetd service successfully
2. See nwg-hello login screen
3. Authenticate with U2F hardware keys
4. Launch Sway session correctly
5. Maintain all current functionality

## Resource Impact After Migration

Expected system improvement:
- **Memory savings**: ~200MB (from 222MB GDM processes to ~20MB greetd)
- **CPU reduction**: Eliminate GNOME Shell overhead
- **Boot time**: Faster startup without GNOME initialization
- **Architecture**: Clean Wayland-native login flow

---

**Next Action**: Run the research commands above to determine installation approach