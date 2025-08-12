# Fedora to NixOS System Inventory

**Generated**: 2025-08-12
**Purpose**: Complete system documentation for NixOS migration planning

## Hardware Specifications

### CPU
- **Model**: Intel Core i5-6300U @ 2.40GHz (Skylake, 6th gen)
- **Architecture**: x86_64
- **Cores**: 2 physical cores, 4 threads (2 threads per core)
- **Cache**: L1d: 64 KiB, L1i: 64 KiB, L2: 512 KiB, L3: 3 MiB
- **Features**: VT-x virtualization, hardware security mitigations enabled
- **Power**: TDP ~15W (mobile processor)

### Graphics & Display
- **iGPU**: Intel HD Graphics 520 (Skylake GT2)
- **Driver**: Intel i915 (integrated graphics)
- **Wayland Support**: Native via Sway compositor
- **Hardware Acceleration**: Mesa/nixGL required for Nix applications

### Audio
- **Controller**: Intel Sunrise Point-LP HD Audio
- **Current**: PipeWire/ALSA stack
- **Requirements**: Full audio stack configuration needed

### Storage & Filesystems
```
/dev/sda (894GB total)
├── /dev/sda1 (ext4, /boot) - 622MB, 34% used
└── /dev/sda2 (btrfs, /) - 894GB, 49% used (442GB available)
    └── Subvolumes: / and /home on same btrfs filesystem
```
- **Available Space**: 443GB free for NixOS installation
- **Current FS**: Btrfs with subvolume layout
- **Boot**: Traditional MBR/Legacy boot (not UEFI)

### Network Interfaces
- **Ethernet**: enp0s31f6 (ThinkPad docking station)
  - Current IP: 192.168.1.152/16 via DHCP
- **WiFi**: wlp4s0 (Intel wireless)
  - Current IP: 192.168.1.9/16 via DHCP
- **Gateway**: 192.168.0.1 (dual-interface setup)

### USB & Peripherals
- Logitech Nano Receiver (mouse/keyboard)
- ThinkPad Ultra Dock Hub (dual hub configuration)
- Intel Bluetooth interface (8087:0a2b)
- Cooler Master Storm Quick Fire XT keyboard
- Chicony integrated webcam
- Multiple USB 3.0/2.0 hubs via docking station

## Current System Configuration

### Critical System Services
```
Service                  Purpose                    NixOS Equivalent
----------------------- -------------------------- ------------------
NetworkManager          Network management         networking.networkmanager
bluetooth.service       Bluetooth stack           hardware.bluetooth
alsa-state             Audio state management     sound.enable
cups.service           Printing system           services.printing
chronyd                NTP time sync             services.chrony
firewalld              Firewall management       networking.firewall
avahi-daemon           mDNS/service discovery    services.avahi
dbus-broker            System message bus        services.dbus (default)
auditd                 Security audit logging    security.audit
abrtd                  Crash reporting           (disable in NixOS)
```

### User Environment (Already Configured)
- **Shell**: Zsh with custom themes (already in home.nix)
- **Desktop**: Sway + Waybar + Wofi (already configured)
- **Applications**: 95+ packages managed via home-manager
- **Services**: mako, gammastep, log-cleanup (user systemd services)

### Authentication & Security
- **U2F Keys**: Configured for GDM and sudo authentication
- **Key Storage**: `/etc/u2f_mappings` (needs migration to NixOS)
- **Hardware**: Multiple FIDO2/U2F devices for GitHub, GitLab, etc.
- **Current Users**: Single user `pavlos` with sudo privileges

### Network Configuration
- **DHCP**: Both interfaces using NetworkManager DHCP
- **DNS**: Automatic via DHCP (likely systemd-resolved)
- **Routing**: Dual interface with metric-based priority
- **VPN**: Not currently configured

## Migration Requirements

### Essential NixOS Modules Required
```nix
# Core hardware support
hardware.cpu.intel.updateMicrocode = true;
hardware.opengl.enable = true;
hardware.bluetooth.enable = true;

# Network management
networking.networkmanager.enable = true;
networking.useDHCP = false;
networking.interfaces = {
  enp0s31f6.useDHCP = true;
  wlp4s0.useDHCP = true;
};

# Audio stack
sound.enable = true;
hardware.pulseaudio.enable = false;
services.pipewire = {
  enable = true;
  alsa.enable = true;
  pulse.enable = true;
};

# Security & authentication
security.pam.services.sudo.u2fAuth = true;
security.pam.u2f.cue = true;
```

### Data Migration Strategy
- **Home directory**: Full rsync backup + restore
- **Configuration**: Already in git (home-manager)
- **U2F mappings**: Manual copy from `/etc/u2f_mappings`
- **System configs**: Document and recreate declaratively

### Boot Configuration
- **Current**: GRUB legacy boot on MBR
- **NixOS**: Configure boot.loader.grub with proper device
- **Dual boot**: Possible during testing phase

## Risk Assessment

### High Risk Areas
1. **Network connectivity**: NetworkManager must work immediately
2. **Graphics**: Intel graphics + nixGL integration critical
3. **Authentication**: U2F key mapping must transfer correctly
4. **Boot**: Legacy MBR setup requires careful GRUB configuration

### Mitigation Strategies
1. **Test in VM first**: Full configuration validation
2. **Parallel installation**: Keep Fedora bootable during testing
3. **Network fallback**: Document manual network configuration
4. **Recovery plan**: Bootable USB with SSH access

### Low Risk Areas
- User applications (already managed by home-manager)
- Desktop environment (Sway already configured)
- Development tools (all in Nix packages)
- User services (already declarative)

## Next Steps

1. Create NixOS `configuration.nix` based on this inventory
2. Set up VM testing environment
3. Validate hardware compatibility
4. Test U2F authentication integration
5. Plan partition strategy (dual-boot vs fresh install)

---

**Note**: This system is well-suited for NixOS migration due to:
- Modern Intel hardware with good Linux support
- Already using Wayland/Sway (no X11 dependencies)
- Comprehensive home-manager setup (95% user config done)
- Plenty of disk space for parallel installation
