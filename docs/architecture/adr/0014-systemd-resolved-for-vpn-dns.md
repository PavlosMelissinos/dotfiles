# ADR-0014: systemd-resolved for VPN DNS Failover

**Date**: 2026-10-31
**Status**: Accepted

## Context

With the work OpenVPN tunnel connected, apps that check NetworkManager's
connectivity state (Viber, GNOME/GTK) report "no internet" even though the
network works. Measured on the live system while the VPN was up:

| Check | Result |
|---|---|
| `curl --resolve` to the probe URL (DNS bypassed) | `200` |
| `getent ahostsv4 nmcheck.gnome.org` | 13.1 s |
| `host nmcheck.gnome.org 8.8.8.8` | 0.36 s |
| `host`/`ping` to the DHCP-supplied ISP resolver | fail/timeout |

The VPN profile (`iSprintSupport_pmelissinos_supportserver.ovpn`, in the work
repo) accepts `redirect-gateway` but sets no `script-security`/`up`/`down`
hooks, so it never replaces the Wi-Fi-supplied ISP resolvers in
`resolv.conf`. Those resolvers only answer from inside the ISP's own
network, so from the tunnel they're unreachable. glibc's plain resolver
retries each dead server (~5 s) on every single lookup before falling
through to the `networking.nameservers` fallback — slow enough that
NetworkManager's connectivity probe (ADR pending; see
`networking.networkmanager.settings.connectivity` in `configuration.nix`)
times out and reports `LIMITED`.

The correct fix is in the `.ovpn` profile (an `up`/`down` hook to manage
`resolv.conf`), which lives in a different repo and isn't ours to change.

## Decision

Enable `services.resolved.enable = true;`. On NixOS this also switches
`networking.networkmanager.dns` to `"systemd-resolved"` automatically. It
scopes DNS per link, marks a dead server as failing instead of retrying it
on every lookup, and caches answers — so the cost is paid once per link
instead of on every single DNS query.

## Consequences

### Positive
- The ~13 s DNS stall is paid once after connecting the VPN, not per lookup.
- `networking.nameservers` (`8.8.8.8`, `8.8.4.4`) keeps working as the
  global fallback, now scoped correctly instead of racing dead servers.

### Negative
- `/etc/resolv.conf` becomes the resolved stub (`127.0.0.53`); anything
  parsing it directly instead of using the standard resolver should use
  `resolvectl` instead.

### Follow-up (different repo)
Fix the `.ovpn` profile to manage DNS itself (`script-security 2` plus an
`up`/`down` hook, e.g. `update-systemd-resolved`) so the tunnel points
resolution at a server reachable through it and restores the previous one
on disconnect.
