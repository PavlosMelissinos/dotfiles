# Hardware Configuration

## CRITICAL: hardware-configuration.nix Auto-Generation

**DO NOT create hardware-configuration.nix manually!**

This file MUST be auto-generated during NixOS installation using:
```bash
nixos-generate-config --root /mnt
```

## Why Auto-Generation is Required

1. **Filesystem Type Detection**: Auto-detects actual filesystem types (ext4, btrfs, xfs, etc.)
2. **UUID Mapping**: Uses actual partition UUIDs from your hardware
3. **Hardware Detection**: Properly detects CPU, GPU, and kernel modules needed
4. **Boot Configuration**: Sets up correct bootloader configuration for your system

## Manual Creation Causes These Issues

- Filesystem type mismatches leading to boot failures
- Incorrect UUIDs causing mount failures
- Missing kernel modules for hardware support
- Incompatible bootloader configuration

## Installation Process

The hardware-configuration.nix file will be automatically created when you run:

```bash
# During NixOS installation
nixos-generate-config --root /mnt

# This creates:
# /mnt/etc/nixos/hardware-configuration.nix  (auto-generated)
# /mnt/etc/nixos/configuration.nix           (template to customize)
```

Then copy your custom flake configuration to replace the template configuration.nix.