# Complete Fedora to NixOS Migration Plan

**Date**: 2025-08-12
**Goal**: Achieve full git-tracked system reproducibility through NixOS migration
**Current State**: Fedora 42 with home-manager managing 95+ user packages

## Migration Strategy Overview

This plan provides a **safe, incremental approach** to migrating from Fedora to NixOS while preserving your excellent existing home-manager configuration and achieving complete system reproducibility.

### Prerequisites Completed ✅

- **System inventory documented**: Hardware specs, services, and network config cataloged
- **NixOS configuration created**: System-level `configuration.nix` based on current setup
- **Home-manager setup**: Already managing 95+ packages declaratively via flakes
- **Risk assessment**: Critical areas identified with mitigation strategies

## Phase 1: Preparation & Testing (Safe)

### 1.1 Configuration Validation

**Files to create/verify**:
```
nixos/
├── configuration.nix      # System configuration (✅ created)
├── hardware-configuration.nix  # Hardware template (✅ created)
└── flake.nix             # System flake configuration (pending)
```

**Create system flake**:
```bash
cd ~/.config/nixos
```

Create `flake.nix`:
```nix
{
  description = "NixOS system configuration for pavlos@localhost";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixgl = {
      url = "github:nix-community/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, nixgl }: {
    nixosConfigurations.localhost-nixos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./configuration.nix
        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.pavlos = import ../config/home-manager/home.nix;
          home-manager.extraSpecialArgs = { inherit nixgl; };
        }
      ];
    };
  };
}
```

### 1.2 VM Testing Environment

**Set up QEMU/KVM VM**:
```bash
# Download NixOS ISO (minimal)
wget https://channels.nixos.org/nixos-25.05/latest-nixos-minimal-x86_64-linux.iso

# Create VM disk (20GB sufficient for testing)
qemu-img create -f qcow2 nixos-test.qcow2 20G

# Launch VM with your configs mounted
qemu-system-x86_64 \
  -enable-kvm \
  -m 4G \
  -drive file=nixos-test.qcow2,format=qcow2 \
  -cdrom latest-nixos-minimal-x86_64-linux.iso \
  -virtfs local,path=/home/pavlos/.config/nixos,mount_tag=host0,security_model=passthrough \
  -virtfs local,path=/home/pavlos/.config/home-manager,mount_tag=host1,security_model=passthrough
```

**In VM, test installation**:
```bash
# Mount host directories
mkdir -p /mnt/nixos /mnt/home-manager
mount -t 9p -o trans=virtio,version=9p2000.L host0 /mnt/nixos
mount -t 9p -o trans=virtio,version=9p2000.L host1 /mnt/home-manager

# Standard NixOS installation with your configs
nixos-install --root /mnt --flake /mnt/nixos#localhost-nixos
```

### 1.3 Critical Validation Tests

**Test in VM before real migration**:
- [ ] System boots successfully
- [ ] Network interfaces (ethernet/wifi) work with NetworkManager
- [ ] Sway compositor launches and functions
- [ ] Home-manager applies successfully
- [ ] Graphics acceleration works (test with `glxinfo`)
- [ ] Audio through PipeWire functions
- [ ] Bluetooth pairs devices
- [ ] U2F authentication works (simulate with software keys)

## Phase 2: Data Backup & Safety Net

### 2.1 Complete System Backup

**Create full system backup**:
```bash
# Create backup destination
sudo mkdir -p /backup/fedora-system

# Backup complete system (excluding /proc, /sys, /dev, /tmp)
sudo rsync -aAXv --exclude={"/proc/*","/sys/*","/dev/*","/tmp/*","/run/*","/mnt/*","/media/*","/backup/*"} / /backup/fedora-system/

# Backup package list for reference
dnf list installed > /backup/fedora-packages.txt
rpm -qa > /backup/fedora-rpm-packages.txt

# Backup critical system files
sudo cp /etc/u2f_mappings /backup/u2f_mappings.backup
sudo cp -r /etc/NetworkManager /backup/networkmanager-config
```

**Verify backup integrity**:
```bash
# Test restore capability with a few critical files
sudo rsync -n -aAXv /backup/fedora-system/etc/u2f_mappings /tmp/test-restore
```

### 2.2 Create Recovery Plan

**Bootable rescue USB**: Keep Fedora live USB available
**Network fallback**: Document manual network configuration
**SSH access**: Ensure remote access possible if needed

## Phase 3: Migration Execution

### 3.1 Dual-Boot Installation (Recommended)

**Partition strategy** (443GB available):
```
Current: /dev/sda2 (894GB btrfs, 49% used)
Plan:
├── Keep existing Fedora (shrink to ~300GB)
├── New NixOS root (100GB)
├── Shared /home (remaining space)
```

**Shrink Fedora partition**:
```bash
# Boot from live USB
sudo btrfs filesystem resize -100G /dev/sda2
sudo parted /dev/sda resizepart 2 794G
```

**Create NixOS partitions**:
```bash
sudo parted /dev/sda mkpart primary btrfs 794G 894G
sudo mkfs.btrfs -L nixos-root /dev/sda3
```

### 3.2 NixOS Installation

**Install NixOS with your configs**:
```bash
# Mount new NixOS partition
sudo mount /dev/sda3 /mnt
sudo mount /dev/sda1 /mnt/boot

# Copy your configurations
sudo cp -r /home/pavlos/.config/nixos /mnt/etc/nixos/

# Install with flake configuration
sudo nixos-install --root /mnt --flake /mnt/etc/nixos#localhost-nixos

# Copy U2F mappings
sudo cp /backup/u2f_mappings.backup /mnt/etc/u2f_mappings
```

**Post-installation setup**:
```bash
# Boot into NixOS
# Apply home-manager configuration
cd ~/.config/home-manager
home-manager switch --flake .

# Verify all services
systemctl --user status mako gammastep log-cleanup.timer
```

### 3.3 Migration Validation

**Critical tests after installation**:
- [ ] All hardware functions (network, bluetooth, audio, graphics)
- [ ] U2F authentication works for login and sudo
- [ ] Sway desktop environment fully functional
- [ ] All applications launch correctly
- [ ] Development tools work (emacs, git, languages)
- [ ] System services properly configured

## Phase 4: Finalization

### 4.1 Remove Fedora (Once NixOS Validated)

**Only after 1-2 weeks of successful NixOS usage**:
```bash
# Remove Fedora partition (in NixOS)
sudo parted /dev/sda rm 2  # Old Fedora partition
sudo btrfs filesystem resize max /dev/sda3  # Expand NixOS to full disk
```

### 4.2 Final Repository Structure

**Achieved git-tracked system**:
```
~/.config/home-manager/     # User environment (existing)
├── flake.nix
├── home.nix
└── .zshrc

/etc/nixos/                 # System configuration (new)
├── flake.nix
├── configuration.nix
└── hardware-configuration.nix

~/docs/                     # Documentation
├── migration/
└── architecture/adr/
```

### 4.3 One-Command Restore Capability

**Full system restore process**:
```bash
# 1. Boot NixOS installer
# 2. Clone your dotfiles
git clone <your-repo> /mnt/config

# 3. Install system
nixos-install --root /mnt --flake /mnt/config/nixos#localhost-nixos

# 4. Apply user config
home-manager switch --flake /mnt/config/.config/home-manager
```

## Risk Mitigation

### High-Risk Areas & Solutions

**Network connectivity**:
- Test NetworkManager config thoroughly in VM
- Keep ethernet cable available for fallback
- Document manual `wpa_supplicant` configuration

**U2F authentication**:
- Test with multiple keys during validation
- Keep backup authentication method available
- Verify `/etc/u2f_mappings` format compatibility

**Boot failure**:
- Maintain Fedora bootloader initially (dual boot)
- Keep rescue USB with SSH capability
- Document GRUB recovery procedures

### Rollback Strategy

**If migration fails**:
1. Boot back to Fedora (always available during dual-boot phase)
2. Restore from `/backup/fedora-system` if needed
3. Analyze failure cause and retry with adjustments
4. VM testing helps avoid most failure scenarios

## Success Metrics

✅ **Complete reproducibility**: Both system and user config in git
✅ **One-command restore**: `nixos-install` + `home-manager switch`
✅ **No functionality loss**: All current applications and workflows preserved
✅ **Improved maintenance**: Declarative system configuration
✅ **Enhanced security**: Immutable system base with rollback capability

## Timeline Estimate

- **Phase 1 (VM Testing)**: 2-3 days
- **Phase 2 (Backup)**: 1 day
- **Phase 3 (Installation)**: 1 day
- **Phase 4 (Validation)**: 1-2 weeks
- **Total**: ~2-3 weeks with careful validation

This conservative timeline ensures all functionality is preserved and the migration is completely safe.
