---
title: "Fix Bluetooth br-connection-key-missing for Paired Headsets"
date: 2026-05-12
status: Accepted
category: Operations
---

# PROP-0016: Fix Bluetooth br-connection-key-missing for Paired Headsets

## Context

Connecting to a previously paired Bluetooth headset (QCY Melobuds ANC at
`84:AC:60:F8:4B:94`) via `bluetuith` (run from `nix-shell -p bluetuith`)
fails with:

    Error: Cannot connect to device: br-connection-key-missing

The device appears as "Paired" in `bluetoothctl devices Paired`, but
connection attempts fail. The systemd journal shows repeated "Hands-Free
Voice gateway failed connect" errors (connection refused) for the device.

The Bluetooth daemon (`bluetoothd` 5.86) is running and healthy. The
MX Master 3 mouse (paired to the same adapter) connects without issue.

`/var/lib/bluetooth/` (where BlueZ stores per-device link keys) is on
the persistent btrfs root filesystem — it is not tmpfs-backed.

### Root Cause

BlueZ stores device pairing state in two separate locations:

1. **Device info record** — marks the device as "Paired" / "Trusted" /
   "Bonded" (visible in `bluetoothctl info`).
2. **Link key (cryptographic credential)** — stored in
   `/var/lib/bluetooth/<adapter>/<device>/info`, used to establish
   encrypted connections.

The device info record can persist while the link key goes missing
(deleted, corrupted, never properly stored after pairing). Without
the key, BlueZ refuses the connection with `br-connection-key-missing`
and the kernel-level connection track shows "No matching connection
for device."

Likely trigger: the link key for `84:AC:60:F8:4B:94` under
`/var/lib/bluetooth/34:F3:9A:5E:24:A4/84:AC:60:F8:4B:94/` is absent
or stale. The device may have been paired on a different OS (dual-boot),
or the key was lost during a NixOS rebuild/update.

## Proposal

Resolve the immediate connection failure by removing and re-pairing
the affected device, then implement a backup mechanism to prevent
recurrence.

### Immediate Fix

1. Remove the stale pairing record and re-pair the headset:

   ```bash
   bluetoothctl remove 84:AC:60:F8:4B:94
   # Put headset in pairing mode
   bluetoothctl scan on          # find the device
   bluetoothctl pair 84:AC:60:F8:4B:94
   bluetoothctl connect 84:AC:60:F8:4B:94
   bluetoothctl trust 84:AC:60:F8:4B:94
   ```

### Preventative Measures

2. Add a Bluetooth link key backup script to `~/.local/bin/` that
   copies `/var/lib/bluetooth/` to a dotfiles-tracked backup location
   (e.g., `~/.config/bluetooth/keys-backup/`). Run it after pairing
   new devices and periodically.

3. Verify that `/var/lib/bluetooth/` is not listed in any NixOS
   impermanence exclude list. If using `environment.persistence`,
   ensure `/var/lib/bluetooth` is persisted.

4. Consider adding `bluetuith` to `home.nix` packages (it is
   currently run ad-hoc via `nix-shell -p bluetuith`).

## Consequences

**Positive:**
- Headset connects reliably after re-pairing
- Backup script prevents future key loss from being disruptive
- Tracked in dotfiles, usable after NixOS reinstalls

**Negative:**
- Requires re-pairing the device (putting headset in pairing mode)
- Link key backup contains sensitive cryptographic material (must stay
  local, not committed to public repos)

## Tasks

### Immediate fix
- [x] Remove stale pairing: `bluetoothctl remove 84:AC:60:F8:4B:94`
- [x] Re-pair and trust the headset
- [x] Verify connection succeeds

### Preventative
- [x] Add `bt-backup-keys` script via `writeShellScriptBin` in home.nix
- [x] Verify `/var/lib/bluetooth/` is on persistent btrfs root
- [x] Add `bluetuith` to home.nix packages (replaces ad-hoc nix-shell)

## Alternatives Considered

| Alternative | Reason Not Chosen |
|-------------|-------------------|
| Manually reconstruct key from DMI/bluetoothd debug logs | Not feasible — kernel/hardware-derived keys cannot be reconstructed |
| Use `bluetoothctl --agent` to force re-key | May work but less reliable than clean remove+pair |
| Switch to PipeWire-WirePlumber Bluetooth stack | Does not solve the missing link key problem; key management is BlueZ-level |
| Ignore and use wired connection | Defeats purpose of owning Bluetooth headsets |

## Status History

| Date       | Status   | Notes |
|------------|----------|-------|
| 2026-05-12 | Proposed | Initial analysis of br-connection-key-missing error |
| 2026-06-06 | Accepted | Headset re-paired; bluetuith + bt-backup-keys added to home.nix |
