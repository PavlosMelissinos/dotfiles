#!/usr/bin/env bash

# NixOS Installation Script for VM Migration Testing
# This script automates the entire NixOS installation process that was
# previously done manually during VM testing.

set -euo pipefail

# Configuration variables
TARGET_DISK="/dev/sda"  # Adjust for your target disk (sda for VM, sda for physical)
EFI_SIZE="512M"
SWAP_SIZE="2G"
HOSTNAME="localhost-nixos"
USERNAME="pavlos"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

error() {
    echo -e "${RED}[ERROR]${NC} $*"
    exit 1
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

# Verify we're running as root
if [[ $EUID -ne 0 ]]; then
    error "This script must be run as root. Use: sudo $0"
fi

# Verify target disk exists
if [[ ! -b "$TARGET_DISK" ]]; then
    error "Target disk $TARGET_DISK does not exist!"
fi

# Warning about data destruction
warn "This will DESTROY ALL DATA on $TARGET_DISK!"
echo -n "Are you sure you want to continue? (type 'yes' to confirm): "
read -r confirmation
if [[ "$confirmation" != "yes" ]]; then
    error "Installation cancelled."
fi

log "Starting NixOS installation on $TARGET_DISK"

# Step 1: Partition the disk
log "Creating partitions on $TARGET_DISK"
parted -s "$TARGET_DISK" mklabel gpt
parted -s "$TARGET_DISK" mkpart ESP fat32 1MiB "$EFI_SIZE"
parted -s "$TARGET_DISK" mkpart swap linux-swap "$EFI_SIZE" "$(( ${EFI_SIZE%M} + ${SWAP_SIZE%G} * 1024 ))MiB"
parted -s "$TARGET_DISK" mkpart root ext4 "$(( ${EFI_SIZE%M} + ${SWAP_SIZE%G} * 1024 ))MiB" 100%
parted -s "$TARGET_DISK" set 1 esp on

# Wait for partition devices to be ready
sleep 2

# Determine partition devices
if [[ "$TARGET_DISK" == *"nvme"* ]]; then
    EFI_PART="${TARGET_DISK}p1"
    SWAP_PART="${TARGET_DISK}p2"
    ROOT_PART="${TARGET_DISK}p3"
else
    EFI_PART="${TARGET_DISK}1"
    SWAP_PART="${TARGET_DISK}2"
    ROOT_PART="${TARGET_DISK}3"
fi

log "Partition layout:"
log "  EFI:  $EFI_PART ($EFI_SIZE)"
log "  SWAP: $SWAP_PART ($SWAP_SIZE)"
log "  ROOT: $ROOT_PART (remaining space)"

# Step 2: Format partitions
log "Formatting partitions"
mkfs.fat -F 32 -n BOOT "$EFI_PART"
mkswap -L swap "$SWAP_PART"
mkfs.ext4 -L root "$ROOT_PART"

# Step 3: Mount filesystems
log "Mounting filesystems"
mount "$ROOT_PART" /mnt
mkdir -p /mnt/boot
mount "$EFI_PART" /mnt/boot
swapon "$SWAP_PART"

# Step 4: Generate hardware configuration
log "Generating hardware configuration"
nixos-generate-config --root /mnt

# Verify hardware-configuration.nix was created correctly
if [[ ! -f /mnt/etc/nixos/hardware-configuration.nix ]]; then
    error "Failed to generate hardware-configuration.nix"
fi

# Quick check that it has the correct filesystem types
if ! grep -q "ext4" /mnt/etc/nixos/hardware-configuration.nix; then
    warn "hardware-configuration.nix may not have detected ext4 filesystem correctly"
fi

log "Hardware configuration generated successfully"

# Step 5: Copy our custom configuration
log "Copying custom NixOS configuration"

# Check if we have the configuration directory
NIXOS_CONFIG_DIR="/home/pavlos/.config/nixos"
if [[ ! -d "$NIXOS_CONFIG_DIR" ]]; then
    error "NixOS configuration directory not found at $NIXOS_CONFIG_DIR"
fi

# Copy our configuration files
cp "$NIXOS_CONFIG_DIR/configuration.nix" /mnt/etc/nixos/
cp "$NIXOS_CONFIG_DIR/flake.nix" /mnt/etc/nixos/

# Also need home-manager configuration
HOME_MANAGER_CONFIG_DIR="/home/pavlos/.config/home-manager"
if [[ -d "$HOME_MANAGER_CONFIG_DIR" ]]; then
    log "Copying home-manager configuration"
    mkdir -p /mnt/home/pavlos/.config
    cp -r "$HOME_MANAGER_CONFIG_DIR" /mnt/home/pavlos/.config/
    chown -R 1000:1000 /mnt/home/pavlos
else
    warn "home-manager configuration not found, will need to be copied manually after installation"
fi

log "Configuration files copied successfully"

# Step 6: Run NixOS installation
log "Running NixOS installation (this may take several minutes)"
nixos-install --root /mnt --flake /mnt/etc/nixos#localhost-nixos --no-root-passwd

success "NixOS installation completed successfully!"
log ""
log "Next steps:"
log "1. Reboot into your new NixOS system"
log "2. Login as user '$USERNAME' with password 'temppass123'"
log "3. Change your password: passwd"
log "4. If home-manager wasn't copied, run:"
log "   mkdir -p ~/.config && scp -r user@host:~/.config/home-manager ~/.config/"
log "   cd ~/.config/home-manager && home-manager switch --flake ."

log ""
warn "The system is ready for reboot. Run 'reboot' to start your new NixOS system."
