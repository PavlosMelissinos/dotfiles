---
title: "Fix and Modernize the Caps/Num Lock Indicator"
date: 2026-10-14
status: Proposed
category: Sway
---

# PROP-0019: Fix and Modernize the Caps/Num Lock Indicator

## Context

The waybar `custom/lockkeys` module is broken. It runs
`.config/home-manager/configs/waybar/lockkeys.sh`, which reads:

```sh
capslock_active=`cat /sys/class/leds/input3::capslock/brightness`
numlock_active=`cat /sys/class/leds/input3::numlock/brightness`
```

`input3::*` does not exist on this machine anymore — the kernel renumbers
input devices across boots, and the current keyboards are `input0` (built-in),
`input1` (external CM Storm QuickFire XT), and `input4` (ITE Tech). Every
`cat` fails silently, both variables end up empty, and the comparison logic
(empty != `"0"`) makes the module show an incorrect/stuck state. The script
also has a quoting bug (line 17: `"$tooltip"`/`"$class"` are inside single
quotes, so they're emitted as literal text, and neither variable is even
defined) and prints nothing when both locks are off (line 16), leaving
stale bar output.

Waybar (v0.15.0, already installed) ships a built-in `keyboard-state`
module that is event-driven (no polling) and handles this correctly. It is
already half-configured in this repo's `waybar/config` (lines ~129-137:
`numlock: true`, `capslock: true`, format/icons defined) but was never
added to any `modules-*` array, so it currently does nothing.

Live-checked: all three keyboards report identical lock state (sway groups
keyboards with the same layout), so no `device-path` pinning is needed.
Permissions are already correct — `keyboard-state` reads `/dev/input/event*`,
which needs the `input` group, and the user is already in it
(`nixos/configuration.nix`).

Beyond fixing the bug, a more modern, GNOME/KDE-style on-screen pop-up on
toggle is wanted — SwayOSD provides this and needs no NixOS-level changes
(its libinput backend isn't required; sway can report the key presses
itself via `bindsym --release`).

See also PROP-0003 (Sway Configuration Improvements), which already listed
SwayOSD as a proposed wob replacement for volume/brightness; this proposal
only uses SwayOSD for lock-key pop-ups and leaves the existing wob
volume/brightness pipe untouched.

## Proposal

### Bar indicator (home-manager only)

**`.config/home-manager/configs/waybar/config`**
- In `modules-right`, replace `"custom/lockkeys"` with `"keyboard-state"`.
- Delete the `"custom/lockkeys"` module definition; fix the resulting
  trailing comma.
- Update the existing `"keyboard-state"` block: keep `numlock`/`capslock`
  true, `scrolllock` false, add a per-key `"format"` (e.g.
  `{ "capslock": "⇪ {icon}", "numlock": "NUM {icon}" }`), keep the
  locked/unlocked `format-icons`. No `device-path` needed.

**`.config/home-manager/configs/waybar/style.css`**
- Swap the `#custom-lockkeys` selector for `#keyboard-state`.
- Add `label.locked` (highlighted) and `label:not(.locked)` (dimmed, ~0.4
  opacity) rules so an active lock visually stands out.

**Delete** `.config/home-manager/configs/waybar/lockkeys.sh` via `git rm`
(nothing else references it).

### SwayOSD pop-ups (home-manager only)

**`.config/home-manager/home.nix`**
- Add `services.swayosd.enable = true;` — the home-manager module runs
  `swayosd-server` as a user service under `graphical-session.target`,
  the same pattern already used for mako.

**`.config/home-manager/configs/sway/config`**, near the existing wob
bindings:
```
bindsym --release Caps_Lock exec swayosd-client --caps-lock
bindsym --release Num_Lock  exec swayosd-client --num-lock
```

wob (volume/brightness OSD) is left as-is.

## Tasks

- [ ] Swap `custom/lockkeys` for `keyboard-state` in waybar config/CSS
- [ ] `git rm` the broken `lockkeys.sh`
- [ ] Add `services.swayosd.enable = true;` to `home.nix`
- [ ] Add Caps/Num Lock `swayosd-client` bindings to sway config
- [ ] `home-manager switch --flake .`, `swaymsg reload`
- [ ] Verify per the steps below

## Verification

No reboot needed:

1. `home-manager switch --flake .`, then `swaymsg reload` (waybar runs via
   `bar { swaybar_command waybar }`, not a systemd unit, so reload
   restarts it).
2. Toggle Caps Lock / Num Lock on each keyboard; confirm the bar label
   flips immediately and matches
   `cat /sys/class/leds/input*::capslock/brightness`.
3. `pgrep -fa lockkeys` should print nothing (script is gone).
4. `systemctl --user status swayosd`; press Caps Lock and confirm a
   pop-up appears (`journalctl --user -u swayosd -f` while testing, or run
   `swayosd-client --caps-lock` directly if nothing shows).

## Consequences

**Positive:**
- Fixes a module that's been silently wrong regardless of actual lock
  state.
- Event-driven instead of polling every second.
- No longer depends on a hardcoded, boot-unstable device number.
- Adds a modern visual toggle indicator matching other modern DEs.

**Negative:**
- One more user service (`swayosd`) running, though lightweight and
  following an existing pattern (mako).
- Num Lock is forced on at startup (`xkb_numlock enable`), so its
  indicator will normally read "on" — expected, not a new issue.

## Alternatives Considered

| Alternative | Reason Not Chosen |
|-------------|-------------------|
| Patch `lockkeys.sh`'s hardcoded `input3` to the current device number | Fixes the bug today but breaks again the next time the kernel renumbers input devices (exactly how it broke this time); doesn't address "more modern alternative" |
| Custom notify-send/makoctl script listening to libinput/evdev events | Most code to write and maintain, no benefit over SwayOSD |
| SwayOSD's libinput backend (system-level service) | Not needed — sway can report lock-key presses directly via `bindsym --release`, avoiding NixOS-level service/polkit/udev wiring |

## Status History

| Date       | Status     | Notes |
|------------|------------|-------|
| 2026-10-14 | Proposed   | Confirmed live: all 3 keyboards report identical lock state; `input` group membership already correct |
