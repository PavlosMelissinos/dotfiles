---
title: "Self-Hosted Photo Storage: Immich on Synology DS218+"
date: 2026-04-29
status: Proposed
category: Storage
---

# PROP-0004: Self-Hosted Photo Storage: Immich on Synology DS218+

## Context

Google One storage is 87% full (87.26 GB of 100 GB). The current tier
offers no middle ground between 200 GB ($2.99/mo) and 2 TB ($9.99/mo).
Needing, for example, 300 GB forces a 3.3x price jump to the 2 TB plan.

A Synology DS218+ is already available in the home network:
- Intel Celeron J3355 (2 core, 2.0 GHz)
- 6 GB RAM (upgraded from stock 2 GB)
- 4 TB total, ~1 TB free
- Already running 24/7

This changes the economics entirely. Rather than paying ~EUR 7.19/month
for a Hetzner VPS + Storage Box, the existing NAS can host Immich for
~1.50/mo in additional electricity. Hetzner becomes a secondary option
only if the NAS proves insufficient.

The use case is backup and photo management. The primary tool is Immich,
an open-source self-hosted Google Photos alternative with facial
recognition, object search, timeline views, and mobile apps.

## Synology DS218+ Capability Assessment

| Factor          | Spec                                            | Immich requirement | Verdict        |
|-----------------|-------------------------------------------------|--------------------|----------------|
| CPU             | Celeron J3355 (2 core, 2.0 GHz, Passmark ~1300) | 2+ cores           | Adequate       |
| RAM             | 6 GB (upgraded)                                 | 4 GB minimum       | Satisfied      |
| Architecture    | x86_64, Intel HD Graphics 500                   | x86_64             | Satisfied      |
| ML acceleration | OpenVINO via iGPU (HD Graphics 500)             | Hardware ML        | Available      |
| OS              | DSM 7.x with Container Manager                  | Docker             | Official guide |
| Storage         | ~1 TB free of 4 TB                              | 200 GB+ free       | Satisfied      |

### ML container on J3355

The Intel HD Graphics 500 iGPU supports OpenVINO, enabling hardware-accelerated
face detection and object recognition. A bug affecting this CPU was fixed in
Immich v1.115.0. Initial import of 87 GB will be CPU/ML-intensive (expect
hours to days), but incremental new uploads process in seconds. Set
`MACHINE_LEARNING_WORKER_TIMEOUT=600` in the Immich environment to handle
longer ML jobs without timeout.

RAM usage during heavy ML workloads peaks around 5 GB total across containers.
The 6 GB upgrade provides comfortable headroom.

### Synology-specific setup notes

```bash
# Key configuration for DS218+ Immich deployment:
# 1. Map videodriver group for /dev/dri passthrough (iGPU access)
# 2. Set DB_STORAGE_TYPE=HDD (Synology defaults to SSD-optimized mode)
# 3. Configure firewall for container IP on port 2283
# 4. Set MACHINE_LEARNING_WORKER_TIMEOUT=600
# 5. Use Tailscale for remote access (already configured on system)
```

Official Synology installation guide applies via Container Manager Projects.

## The Pricing Gap

Google One has a structural gap: no tier between 200 GB and 2 TB.
Self-hosting on an existing NAS fills this gap at the lowest cost.

```
Google One Tiers vs Self-Hosted Options
─────────────────────────────────────────────────────────────────
Plan               Storage  Monthly    Annual    Notes
─────────────────────────────────────────────────────────────────
Google One 100 GB   100 GB   $1.99      $23.88    Current (nearly full)
Google One 200 GB   200 GB   $2.99      $35.88    Next tier (forced jump)
Google One 2 TB     2 TB     $9.99      $119.88   Massive overprovision
─────────────────────────────────────────────────────────────────
NAS self-hosted     ~1 TB   ~$1.50     $18.00    Electricity only
Hetzner self-hosted ~1 TB   ~EUR 7.19  ~EUR 86.28  VPS + Storage Box
─────────────────────────────────────────────────────────────────

Break-even vs Google One 2 TB:
  ✓ NAS self-hosted: cheaper from month 1 ($1.50 vs $9.99)
  ✓ Hetzner self-hosted: cheaper from month 1 (EUR 7.19 vs $9.99)
  ✓ NAS is 4.8x cheaper than Hetzner at current electricity costs
  ✓ NAS wins unless existing hardware is unavailable
```

## Architecture

### Components (DS218+)

```
Immich Mobile App (Android/iOS)
       │
       ▼
┌─────────────────────────────────────┐
│  Tailscale VPN (remote access)       │
│  - Synology already has Tailscale    │
│  - Connects from anywhere            │
└──────────────┬──────────────────────┘
               │
               ▼
┌─────────────────────────────────────┐
│  Synology DS218+ (local network)     │
│  - Intel Celeron J3355 / 6 GB RAM   │
│  - DSM 7.x with Container Manager   │
│  - Immich (Docker/Container Manager) │
│  - PostgreSQL 16                    │
│  - Redis 7                          │
│  - Intel HD Graphics 500 (iGPU)     │
│  - Photos library: ~1 TB free       │
└──────────────┬──────────────────────┘
               │
               ▼
┌─────────────────────────────────────┐
│  Backblaze B2 (off-site backup)     │
│  - ~120 GB (photos + thumbs + DB)   │
│  - rclone sync (automated)          │
│  - Cold storage USB drive (manual)  │
└─────────────────────────────────────┘
```

### Why DS218+ over Hetzner

The Synology was already purchased and runs 24/7. The marginal cost of adding
Immich is electricity (~15W idle, spiking during ML workloads). Hetzner
requires paying for a VPS and Storage Box every month from scratch.

### Tailscale for remote access

Tailscale is already configured on the system. The Synology's Tailscale
client provides direct VPN access to the NAS from anywhere. No port forwarding,
no dynamic DNS, no exposed services. The Immich web UI and mobile app connect
through the VPN just like on the local network.

### Off-site backup strategy

Primary storage is on the NAS. Off-site backup via Backblaze B2 protects
against fire, flood, theft:

```
Backblaze B2 for off-site backup:
  87 GB raw photos
  + ~20 GB thumbnails and transcodes (Immich grows storage)
  + ~10 GB Postgres data
  = ~120 GB total backup footprint
  @ EUR 0.006/GB/mo = ~EUR 0.72/mo

Alternative: USB external drive (cold storage)
  - One-time $50-80 for 2-4 TB external drive
  - Manual: connect drive, run rclone sync, store off-site
  - No ongoing cost, but requires manual intervention
```

rclone runs on the NAS (Docker container or native binary) on a schedule.
For simplicity, B2 automated sync is recommended.

## Cost Analysis

```
Monthly breakdown
─────────────────────────────────────────────────────────────────
Component              Monthly    Annual    Notes
─────────────────────────────────────────────────────────────────
Electricity (DS218+)   ~$1.50    $18.00    15W idle, spiking during ML
Tailscale              $0.00     $0.00     Already configured
B2 off-site backup     ~$0.72    $8.64     ~120 GB stored (EUR 0.006/GB)
─────────────────────────────────────────────────────────────────
Total (with B2)        ~$2.22/mo ~$26.64   Off-site redundancy included
Total (B2 optional)    ~$1.50/mo ~$18.00   Local only
─────────────────────────────────────────────────────────────────

Hetzner comparison (EUR/USD reference)
─────────────────────────────────────────────────────────────────
DS218+ self-hosted     ~$2.22/mo  ~$26.64   With B2 backup
Hetzner self-hosted     ~EUR 7.19  ~EUR 86.28  VPS + Storage Box
Google One 2 TB         $9.99/mo   $119.88
─────────────────────────────────────────────────────────────────
DS218+ saves 69% vs Hetzner, 78% vs Google One 2 TB
```

### Hidden costs

- *RAM headroom*: 6 GB is sufficient but tight during ML batch jobs.
  Keep monitoring. The J3355 stock cooler is adequate for the workload.
- *Storage growth*: ~1 TB free. Immich grows storage by ~20-30% for
  thumbnails and transcodes. 87 GB raw becomes ~110 GB on-disk. Leaves
  ~900 GB headroom -- room for years of growth.
- *Drive failure*: The DS218+ uses RAID 1 (mirrored). One drive can fail
  without data loss. Replace failed drive promptly.
- *Time investment*: Initial setup 2-4 hours. Ongoing maintenance ~1 hour/month.

## What You Gain and Lose

```
┌─────────────────────────────────┬─────────────────────────────────┐
│  GAIN                            │  LOSE                           │
├─────────────────────────────────┼─────────────────────────────────┤
│ Privacy: Data stays in your      │ Zero-maintenance: Google handles│
│ control, no Google scanning      │ updates, scaling, auth infra    │
│                                  │                                 │
│ ~$2.22/mo (vs $9.99 Google One)  │ Google Photos AI maturity:      │
│ Lowest self-hosted cost          │ face recognition and object     │
│                                  │ search work but are slower on   │
│                                  │ J3355 iGPU during batch jobs    │
│                                  │ (days vs hours for initial ML)  │
│                                  │                                 │
│ Full photo management UI         │ Android OS-level photo backup:  │
│ (timeline, albums, sharing)      │ must use Immich mobile app      │
│                                  │ instead of system Photos sync   │
│                                  │                                 │
│ Remote access via Tailscale      │ Public URL sharing requires      │
│ (already configured)            │ Tailscale connection            │
│                                  │                                 │
│ B2 off-site backup included      │                                 │
│ at ~$0.72/mo                    │                                 │
└─────────────────────────────────┴─────────────────────────────────┘
```

## Ongoing Commitment

Immich is actively developed (releases every 2-4 weeks). Breaking changes
happen 2-3x per year. Plan for:

### Weekly (5-10 minutes)
- Monitor disk usage on NAS (DSM Storage Manager)
- Check Immich web UI for update notifications

### Monthly (30-45 minutes)
- Run Immich updates via Container Manager
- Review rclone B2 sync logs
- Verify backup integrity (test one restore if possible)

### Quarterly (1 hour)
- Major version upgrades (Immich v1.x → v2.x)
- Review Tailscale ACLs and connected devices
- Check storage growth trajectory against free space

### Dependency chain

```
Immich (Docker) → PostgreSQL → rclone (B2 sync) → Backblaze B2
```

If Immich breaks, photo access is offline until fixed. If B2 backup is
stale, fire/flood/theft results in permanent data loss. Mitigation: regular
B2 sync verification and DSM backup of Docker configuration.

## Alternatives Considered

### Hetzner VPS + Storage Box

The original proposal before the Synology DS218+ was identified as available.

```
Cost: ~EUR 7.19/month (CX22 VPS + BX11 Storage Box)
Pros: Faster CPU (Xeon/EPYC), 1 TB Storage Box, no local hardware needed
Cons: 4.8x more expensive than NAS, pays for hardware already owned
```

Hetzner remains the better choice if:
- The NAS is at capacity or unreliable
- The NAS cannot run Immich (CPU/RAM/storage insufficient)
- A public, always-on VPS is preferred over home-network-dependent access

### Other cloud-only alternatives

**Backblaze B2 as primary store** -- Raw file backup at ~EUR 0.52/month for 87 GB.
No photo UI, no face recognition, no timeline, no mobile app. Not a Google
Photos replacement.

**Amazon Photos + Prime** -- Unlimited full-resolution photo storage free with
Amazon Prime ($139/yr). Face recognition, object search, mature mobile app.
Only viable if Prime is already subscribed for other reasons.

**Ente.io** -- E2E encrypted, Google-Photos-like experience, ~EUR 60/yr for
100 GB. More expensive than Google One for equivalent storage.

### Other local alternatives

**pCloud lifetime 500 GB** -- One-time EUR 199. No photo management UI, privacy
concerns (Bulgaria, closed-source). Not a Google Photos alternative.

**Synology Photos** -- Synology's native photo app. Simpler than Immich but no
face recognition (as of DSM 7.x), no mobile app with auto-upload, no
semantic search. Only viable if Immich is too complex.

## Two Paths Forward

### Path A: Stay on Google One 200 GB ($2.99/month)

Defer the decision. Clean up the photo library (duplicates, blurry photos,
compressed originals where acceptable). At 87 GB, 200 GB gives ~2 years of
headroom at current growth. Kick the problem down the road.

**Pros:** Zero setup, no maintenance, full Google Photos feature set.

**Cons:** No headroom, forced to $9.99/mo when 200 GB fills.

### Path B: Self-host Immich on DS218+ (~$2.22/month)

Install Immich via Container Manager on the existing Synology. Add B2 off-site
backup. Subscribe to ~1 hour/month maintenance. Gain ~1 TB usable storage
(900 GB free after Immich overhead) for less than Google One 200 GB.

**Pros:** 5x storage for less than Google One 200 GB, full photo management
suite, privacy, no new hardware cost.

**Cons:** Initial ML batch import is slow (days on J3355), maintenance is not
zero, losing Android OS-level integration.

### Decision framework

```
Current usage: 87 GB on Google One
Path A: $2.99/mo for 200 GB
Path B (NAS): ~$2.22/mo for ~900 GB free

Break-even: Path B wins on cost at any meaningful storage growth.
            Path A only wins if setup effort must be zero and
            library growth is negligible.

Recommendation:
  - If library is stable (<20 GB/year new photos): Path A
  - If library grows >50 GB/year: Path B
  - If DS218+ is reliable and always-on: Path B
  - If maintenance time is unacceptable: Path A
  - If privacy is a priority: Path B
```

## Summary

The Synology DS218+ already present in the home network can host Immich for
~$2.22/month (electricity + B2 backup), making it 4.8x cheaper than the
Hetzner VPS path and 78% cheaper than Google One 2 TB. With 6 GB RAM and
Intel HD Graphics 500 iGPU, the hardware is adequate -- ML batch jobs will be
slow but functional.

The Hetzner path remains valid as a fallback if the NAS is unavailable,
insufficient, or if a public VPS is preferred over home-network-dependent
access.

The document is complete. Ready for decision: Path A (stay on Google One
200 GB, defer) or Path B (commit to self-hosted Immich on DS218+).

## Status History

| Date       | Status   | Notes                                                                                             |
|------------|----------|---------------------------------------------------------------------------------------------------|
| 2026-04-29 | Proposed | Initial draft (Hetzner VPS + Storage Box)                                                         |
| 2026-05-02 | Proposed | Expanded alternatives, two-path decision framework                                                |
| 2026-05-02 | Proposed | DS218+ identified as available hardware; NAS becomes primary path, Hetzner demoted to alternative |
