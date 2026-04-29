---
title: "Self-Hosted Photo Storage: Immich on Hetzner"
date: 2026-04-29
status: Proposed
category: Storage
---

# PROP-0004: Self-Hosted Photo Storage: Immich on Hetzner

## Context

Google One storage is 87% full (87.26 GB of 100 GB). The current tier
offers no middle ground between 200 GB ($2.99/mo) and 2 TB ($9.99/mo).
Needing, for example, 300 GB forces a 3.3x price jump to the 2 TB plan.
Self-hosting via a Hetzner VPS + Storage Box fills this gap, providing
~1 TB of usable photo storage for roughly EUR 7.20/month -- cheaper than
the 2 TB Google One plan while avoiding the forced tier upgrade.

The use case is backup and photo management. The primary tool is Immich,
an open-source self-hosted Google Photos alternative with facial
recognition, object search, timeline views, and mobile apps.

## The Pricing Gap

Google One has a structural gap: no tier between 200 GB and 2 TB.
Self-hosting on Hetzner fills it cost-effectively.

```
Google One Tiers vs Hetzner Self-Hosted
──────────────────────────────────────────────────────────────────
Plan               Storage  Monthly    Annual    Notes
──────────────────────────────────────────────────────────────────
Google One 100 GB   100 GB   $1.99      $23.88    Current (nearly full)
Google One 200 GB   200 GB   $2.99      $35.88    Next tier (forced jump)
Google One 2 TB     2 TB     $9.99      $119.88   Massive overprovision
──────────────────────────────────────────────────────────────────
Hetzner CX22        80 GB     EUR 3.99  EUR 47.88  VPS (Immich + DB)
Hetzner BX11        1 TB      EUR 3.20  EUR 38.40  Storage Box (photo lib)
──────────────────────────────────────────────────────────────────
Self-hosted total   ~1 TB     ~EUR 7.19 ~EUR 86.28  All-in
──────────────────────────────────────────────────────────────────

Break-even vs Google One 2 TB:
  ✓ Cheaper from month 1 (EUR 7.19 vs $9.99)
  ✓ Breaks even with Google One 2 TB after ~10 months (cumulative)
  ✓ At 87 GB, Google One 200 GB ($2.99/mo) still cheaper short-term
  ✓ Self-hosting wins when storage needs exceed ~500 GB
```

## Architecture

### Components

```
Immich Mobile App (Android/iOS)
       │
       ▼
┌─────────────────────────────────────┐
│  Tailscale VPN (mesh network)       │
│  - Endpoint: <user>.immich.ts.net   │
│  - Auth: MagicLink or SSH key       │
└──────────────┬──────────────────────┘
               │
               ▼
┌─────────────────────────────────────┐
│  Hetzner CX22 VPS                   │
│  - 2 vCPU / 4 GB RAM                │
│  - Ubuntu 24.04 LTS                 │
│  - Immich application server         │
│  - PostgreSQL 16 (Immich DB)        │
│  - Redis 7 (asset caching)          │
└──────────────┬──────────────────────┘
               │
      ┌────────┴────────┐
      ▼                 ▼
┌──────────────┐   ┌──────────────────┐
│  Hetzner BX11 │   │  Backblaze B2    │
│  Storage Box  │   │  (optional)     │
│  1 TB / POSIX│   │  EUR 0.006/GB/mo │
│  SFTP/rsync   │   │  Off-site backup│
└──────────────┘   └──────────────────┘
```

### Why Storage Box over Object Storage

Immich requires a POSIX filesystem -- it performs in-place image
processing (thumbnails, transcoding, live photos) and uses PostgreSQL
for metadata. Object storage (S3) cannot serve as Immich's primary
library store without significant latency and locking issues.

The Hetzner Storage Box (BX11, 1 TB) is a POSIX-accessible network
drive accessible via SFTP/rsync/SMB, making it ideal as Immich's primary
library store.

Backblaze B2 is reserved as an optional off-site backup tier -- not
primary storage. B2's ~$0.52/month cost for 87 GB makes it attractive
for syncing a secondary copy via rclone, but Immich itself must run
against local or POSIX-connected storage.

### Tailscale instead of exposed services

The VPS runs Immich behind Tailscale's VPN mesh. No ports are exposed
to the public internet. Clients connect via `<user>.immich.ts.net`,
authenticating through Tailscale's identity layer. This eliminates the
need for:

- A registered domain name
- Let's Encrypt / ACME certificates
- Firewall rules beyond Tailscale's ACL
- nginx/caddy reverse proxy

The only egress is through Tailscale's exit nodes if configured, or
direct to the Hetzner datacenter for Storage Box I/O.

## Cost Analysis

```
Monthly breakdown (Billed in EUR, USD reference only)
─────────────────────────────────────────────────────────
Component          Plan       Monthly    Annual    Notes
─────────────────────────────────────────────────────────
VPS                CX22       EUR 3.99   EUR 47.88  2 vCPU, 4 GB RAM,
                                                 40 GB NVMe system disk
Storage Box        BX11 1 TB  EUR 3.20   EUR 38.40  10 snapshots included,
                                                 SFTP/rsync access
Tailscale           Free tier  $0.00     $0.00     Mesh VPN, no domain needed
─────────────────────────────────────────────────────────
Total                         EUR 7.19/mo ~EUR 86.28 Excluding optional B2 backup
─────────────────────────────────────────────────────────

Optional: Off-site backup via Backblaze B2
─────────────────────────────────────────────────────────
87 GB photos       B2         ~EUR 0.52  ~EUR 6.24  EUR 0.006/GB/mo
Total with B2                  EUR 7.71/mo ~EUR 92.52  Off-site redundancy
─────────────────────────────────────────────────────────

Google One comparison (USD, approximate)
─────────────────────────────────────────────────────────
Plan               Monthly    Annual
2 TB               $9.99      $119.88
Self-hosted        EUR 7.19    ~EUR 86.28  ~28% cheaper
─────────────────────────────────────────────────────────
```

### Hidden costs

- *Drive failure on VPS*: The CX22 uses cloud block storage (not local).
  System disk is durable; no separate backup needed for the OS itself.
  Immich application state is in PostgreSQL on the system disk.
- *Storage Box snapshots*: 10 included. Each snapshot is a full
  point-in-time copy. Beyond 10 requires either manual rotation or
  accepting data retention limits.
- *Bandwidth*: Hetzner datacenter outbound free. Storage Box has no
  traffic limits. VPS network is generous (unmetered on CX line).
- *Time investment*: Initial setup 2-4 hours. Ongoing maintenance ~1-2
  hours/month (updates, monitoring, backups).

## What You Gain and Lose

```
┌─────────────────────────────────┬─────────────────────────────────┐
│  GAIN                            │  LOSE                           │
├─────────────────────────────────┼─────────────────────────────────┤
│ Privacy: Data stays in your     │ Zero-maintenance: Google handles│
│ control, no Google scanning      │ updates, scaling, auth infra    │
│                                  │                                 │
│ 1 TB headroom (vs 200 GB cap)    │ Google Photos AI: face          │
│ No forced tier jumps             │ recognition and object search   │
│                                  │ are less mature in Immich        │
│ No vendor lock-in                │ (improving rapidly)             │
│                                  │                                 │
│ Full photo management UI         │ Android OS-level photo backup   │
│ (timeline, albums, sharing)      │ integration (must use Immich    │
│                                  │  mobile app instead)            │
│ Tailscale-only access            │ Public URL / sharing links      │
│ (no exposed internet ports)      │ (requires Tailscale login)      │
│                                  │                                 │
│ B2 backup optionality            │ Automatic deduplication         │
│                                  │ (Google handles this for free) │
└─────────────────────────────────┴─────────────────────────────────┘
```

The tradeoffs are acceptable for a backup-first use case where privacy
and cost savings outweigh OS-level Android integration.

## Ongoing Commitment

Immich is actively developed (releases every 2-4 weeks). Breaking
changes happen 2-3x per year, sometimes requiring database migrations
or config updates. Plan for:

### Weekly (5-10 minutes)

- Monitor disk usage on VPS and Storage Box
- Check Immich web UI for update notifications

### Monthly (30-60 minutes)

- Run Immich updates (usually single command via docker/compose)
- Review PostgreSQL backup logs
- Verify Storage Box snapshots are created correctly

### Quarterly (1-2 hours)

- Major version upgrades (e.g., v1.x → v2.x)
- Review Tailscale ACLs and connected devices
- Test restore from backup (verify B2 or snapshot actually works)

### Dependency chain

```
Docker/Compose → Immich server → PostgreSQL → rclone (B2 sync)
```

If Immich breaks, you lose photo access until fixed. If PostgreSQL
corrupts and B2 backup is stale, you lose data. These risks are
mitigated by regular backup testing and keeping snapshots of the
Storage Box.

## Alternatives Considered

| Alternative                    | Reason Not Chosen                          |
|--------------------------------|--------------------------------------------|
| Backblaze B2 as primary store  | Immich requires POSIX; cannot run DB on S3 |
| Amazon Photos + Prime          | Only free with existing Prime ($139/yr)    |
| Ente.io                        | ~EUR 60/yr for 100 GB, more than Google One|
| Synology NAS                   | Breaks even with 2 TB after ~12 years      |
| Hetzner Object Storage (S3)    | Cannot serve as Immich primary; POSIX req. |
| pCloud lifetime 500 GB         | EUR 199 one-time; no photo UI; privacy    |

The Hetzner + Immich route is the lowest cost path that provides a
full photo management suite (not raw backup), reasonable maintenance
overhead, and privacy without vendor lock-in -- specifically for the
storage range between 200 GB and 2 TB where Google One forces
overprovisioning.

## Summary

The self-hosted Immich on Hetzner is cost-effective at ~1 TB storage
needs. It costs roughly EUR 7.19/month (VPS + Storage Box) versus
Google One's forced 2 TB plan at $9.99/month -- a 28% savings, with
5x the storage. Tailscale keeps everything private with zero exposed
ports.

The tradeoff is ongoing maintenance (~1-2 hours/month) and losing
Google's AI search and OS-level Android integration. If photos are
primarily a backup concern and Immich's feature set is acceptable, the
self-hosted route is the clear financial winner between 500 GB and
several terabytes.

## Status History

| Date       | Status    | Notes |
|------------|-----------|-------|
| 2026-04-29 | Proposed  | Initial draft |