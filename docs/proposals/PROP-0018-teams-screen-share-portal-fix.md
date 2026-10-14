---
title: "Fix Microsoft Teams Screen Sharing Under Sway"
date: 2026-10-14
status: Proposed
category: Sway
---

# PROP-0018: Fix Microsoft Teams Screen Sharing Under Sway

## Context

Screen sharing in `teams-for-linux` (the Electron-based Teams client,
installed via `home.nix`) does not work. The Electron/PipeWire side is
already correct: the running process uses
`--ozone-platform-hint=auto --enable-features=WaylandWindowDecorations,WebRTCPipeWireCapturer`,
and its GPU process runs natively on Wayland.

The actual cause is in the portal layer. NixOS's `programs.sway` module
auto-generates `/etc/xdg/xdg-desktop-portal/sway-portals.conf`:

```
[preferred]
default=gtk
org.freedesktop.impl.portal.ScreenCast=wlr
org.freedesktop.impl.portal.Screenshot=wlr
```

but `xdg-desktop-portal` only reads this file when its own process
environment has `XDG_CURRENT_DESKTOP=sway`. Confirmed live on this system:

- `systemctl --user show-environment` has no `XDG_CURRENT_DESKTOP`.
- The running `xdg-desktop-portal` process's environment lacks it too.
- `journalctl --user -u xdg-desktop-portal` logs "Choosing gtk.portal for
  ... as a last-resort fallback" for every interface.
- `xdg-desktop-portal-wlr.service` is inactive and has never started this
  boot.

The gtk portal backend has no `ScreenCast` implementation, so Teams has
nothing to answer a screen-share request with.

Root cause: `.config/home-manager/configs/sway/config` never includes
`/etc/sway/config.d/*`, so NixOS's own `/etc/sway/config.d/nixos.conf`
(which runs `dbus-update-activation-environment --systemd DISPLAY
WAYLAND_DISPLAY SWAYSOCK XDG_CURRENT_DESKTOP`, imports into systemd, and
manages `sway-session.target`) never runs. The config's own environment
import, at line 154 (`exec systemctl --user import-environment
WAYLAND_DISPLAY XDG_SESSION_TYPE PATH`), updates systemd's manager
environment only (not D-Bus activation) and omits `XDG_CURRENT_DESKTOP`
entirely.

`.config/nixos/configuration.nix`'s `xdg.portal` block (`wlr.enable =
true`, `extraPortals = [ pkgs.xdg-desktop-portal-gtk ]`) is already
correct and needs no change — the portal backends are installed, they
just aren't being selected.

## Proposal

Reuse NixOS's own generated session glue instead of hand-rolling the same
`dbus-update-activation-environment` logic a second time:

**`.config/home-manager/configs/sway/config`**, add near the top (before
`exec systemctl --user start graphical-session.target`):

```
# NixOS-generated session glue: pushes DISPLAY/WAYLAND_DISPLAY/SWAYSOCK/
# XDG_CURRENT_DESKTOP to systemd + D-Bus activation (xdg-desktop-portal
# needs XDG_CURRENT_DESKTOP to route screen-share requests to wlr instead
# of falling back to the gtk portal, which can't capture at all) and
# manages sway-session.target.
include /etc/sway/config.d/*
```

Delete the now-redundant `exec systemctl --user start
graphical-session.target` line — `sway-session.target`
(`BindsTo=graphical-session.target`) starts it anyway, with proper
lifecycle (also stops it on sway exit).

Keep the existing `import-environment WAYLAND_DISPLAY XDG_SESSION_TYPE
PATH` line — it still carries `PATH`, which `nixos.conf` doesn't set.

Not affected: the greeter's sway session (`greeterSwayConfig` in
`configuration.nix`) uses its own separate config file and doesn't need
this.

Also update `docs/architecture/adr/0002-sway-window-manager-wayland-first.md`
(~line 49), replacing the vague "screen sharing requires specific Wayland
protocols" limitation note with the actual mechanism and fix.

## Tasks

- [ ] Add `include /etc/sway/config.d/*` to
      `.config/home-manager/configs/sway/config`
- [ ] Remove the redundant `exec systemctl --user start
      graphical-session.target` line
- [ ] Update ADR-0002's screen-sharing note
- [ ] `home-manager switch --flake .`
- [ ] Verify per the steps below, then confirm with a real Teams call

## Verification

No reboot needed to test the mechanism:

```bash
# Simulate what the new include will do on next login
dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY SWAYSOCK XDG_CURRENT_DESKTOP
systemctl --user show-environment | grep XDG_CURRENT_DESKTOP   # expect =sway

systemctl --user restart xdg-desktop-portal
journalctl --user -u xdg-desktop-portal --since "1 min ago"    # no more "last-resort fallback"

busctl --user get-property org.freedesktop.portal.Desktop \
  /org/freedesktop/portal/desktop org.freedesktop.portal.ScreenCast \
  AvailableSourceTypes                                          # should return a value, not error
```

Then quit and relaunch `teams-for-linux`, start a screen share while
tailing `journalctl --user -u xdg-desktop-portal -u xdg-desktop-portal-wlr
-f` — the wlr output chooser should appear and the share should work in
the call.

After a real logout/login, confirm `XDG_CURRENT_DESKTOP` is set without
any manual step.

## Consequences

**Positive:**
- Fixes screen sharing in Teams (and any other portal-dependent app:
  screenshots, file pickers under wlr, etc.).
- Reuses NixOS's own maintained session glue instead of duplicating it —
  future NixOS fixes to `nixos.conf` apply automatically.
- One-line change, trivially revertible.

**Negative:**
- None identified. The include also starts `sway-session.target`, which
  is a standard part of the sway systemd integration and shouldn't
  surprise anything else already running.

## Alternatives Considered

| Alternative | Reason Not Chosen |
|-------------|-------------------|
| Hand-edit line 154 to add `XDG_CURRENT_DESKTOP` via `dbus-update-activation-environment --systemd` directly | Works, but duplicates what NixOS already generates in `nixos.conf` and will drift from it over time |
| Add `xdg.portal.config.common` in `configuration.nix` | Not sufficient alone — the portal still needs `XDG_CURRENT_DESKTOP` in its environment to read any config section, sway-specific or common; doesn't address the root cause |

## Status History

| Date       | Status     | Notes |
|------------|------------|-------|
| 2026-10-14 | Proposed   | Root cause confirmed live (journalctl, busctl, /proc environment inspection) |
