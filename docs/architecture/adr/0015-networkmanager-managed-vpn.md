# ADR-0015: NetworkManager-Managed VPN Instead of Raw OpenVPN

**Date**: 2026-11-02
**Status**: Accepted

## Context

Viber refuses to work (not cosmetic — messages fail to send) whenever the
work VPN is connected, even though the VPN itself is healthy and every other
app has normal internet access. Confirmed live:

- The VPN runs as a bare `sudo openvpn --config iSprintSupport_...ovpn`
  process. `nmcli device status` shows `tun0` as `connected (externally)` —
  NetworkManager doesn't manage it.
- NetworkManager's global `Connectivity` property is a rollup of each
  *managed* device's own bound connectivity check. With `tun0` excluded,
  the only managed device is Wi-Fi.
- Wi-Fi's own bound check genuinely fails while the VPN's
  `redirect-gateway` is active: `ping`/`curl` bound specifically to
  `wlp0s20f3` time out completely, even though the tunnel itself is fast
  (confirmed: default-route `curl` through `tun0` returns in ~0.24s).
- So the rollup reports `LIMITED`. Viber is the one app that asks
  NetworkManager "are we online?" via its `libqnetworkmanager.so` Qt plugin,
  and believes it.

NetworkManager's own D-Bus reference confirms the mechanism: when a VPN
holds the default route, `PrimaryConnection` shifts to "the connection that
contains the route to the VPN endpoint" — the physical link, not the VPN.

## Decision

Make the VPN a real NetworkManager connection instead of a bare `openvpn`
process:

- `networking.networkmanager.plugins = [ pkgs.networkmanager-openvpn ];`
  so NM can import and run `.ovpn` profiles.
- Autostart `nm-applet` (tray icon; already an installed package via
  `networkmanagerapplet`) and a polkit authentication agent (wrapped from
  the already-installed `polkit_gnome` package, since its binary lives
  under `libexec/` and isn't on `$PATH`) in `configs/sway/config`.
- Import the actual `.ovpn` profile with
  `nmcli connection import type openvpn file ...` (a one-time, manual, live
  step — it creates root-owned connection/secret state under
  `/etc/NetworkManager/system-connections/`, the same place Wi-Fi profiles
  already live uncommitted, so it isn't part of this repo).

Once `tun0` is NM-managed, it gets its own bound connectivity check, which
succeeds, and the global rollup should follow the best device instead of
being stuck on Wi-Fi's VPN-induced `LIMITED` state.

## Consequences

### Positive
- Fixes the false "no internet" for every NM-aware app, not just Viber.
- Connect/disconnect/status via `nm-applet` (tray), `nm-connection-editor`
  (GUI, already wired to waybar's network module), `nmtui`, or `nmcli` —
  all either already installed or added here.
- VPN lifecycle becomes visible in `journalctl -u NetworkManager`, instead
  of whatever terminal happened to run the raw command.

### Negative
- NM's OpenVPN import doesn't necessarily preserve every custom directive
  in the source `.ovpn` (`pull-filter`, `remote-random`,
  `server-poll-timeout`, `setenv`, `sndbuf`/`rcvbuf`) — the imported
  profile needs checking against the original before being trusted, and the
  old `sudo openvpn` command stays as a documented fallback until the
  NM-managed path has been relied on for real for a few sessions.
- One more thing listening for credentials (NM's secret agent via
  gnome-keyring) instead of whatever prompted for the password before.

### Out of scope
- The `.ovpn` profile still has no DNS `up`/`down` hooks — the actual root
  cause behind [ADR-0014](0014-systemd-resolved-for-vpn-dns.md)'s DNS-stall
  workaround. That file lives in a different repo and isn't ours to change.
- Waybar has a half-built, commented-out `custom/vpn` module
  (`configs/waybar/config:12`) for an always-visible bar indicator. Not
  needed — the tray icon covers status/connect/disconnect — but worth
  resurrecting later if wanted.
