---
title: "dotops: Declarative Dotfile Operations Toolkit"
date: 2026-05-03
status: Proposed
category: Tooling
---

# PROP-0015: dotops — Declarative Dotfile Operations Toolkit

## Context

The dotfiles repository contains multiple data sources that require
periodic maintenance: `docs/tech-radar.yml` (technology catalog),
`docs/architecture/adr/` (decision records), and `docs/proposals/`
(change plans). These share structural relationships (ADRs referenced
from tech radar entries, proposals linked from both) but lack tooling
to validate cross-references, regenerate indices, or surface stale
entries.

A tech radar HTML generator is needed to render `docs/tech-radar.yml`
into a visual radar using the Zalando Tech Radar JS library. The
generator naturally lives alongside index generation, health checks, and
document browsing as part of a unified operations toolkit.

Currently, `docs/proposals/gen-index.py` handles proposal index
generation in Python. The remaining operations are manual (grep, ls,
vim).

The user has selected Babashka (Clojure scripting runtime) as the
tooling language and Gum (TUI widget toolkit) for interactive CLI
menus, both available in nixpkgs. Placing the toolkit in a separate
Nix flake repository respects the boundary between declarative
configuration (dotfiles) and imperative operations (tooling).

## Proposal

We will create `dotops` as a separate Nix flake repository
(`github.com/pavlos/dotops`) providing a single package: a Babashka
script with a Gum-powered interactive CLI menu. The dotfiles repository
will consume this package as a home-manager flake input.

### Package contents

The derivation wraps a Babashka project (`bb.edn` + source files) into
a `dotops` binary. Dependencies: `babashka`, `gum`, and `glow` (the
latter for document browsing). At runtime, `dotops` resolves dotfiles
paths relative to `$HOME`.

### Operations

```
dotops (Gum menu)
├── Generate tech radar HTML
│     Reads docs/tech-radar.yml, writes docs/tech-radar/index.html
│     using Zalando's radar.js (CDN-hosted d3.js + radar.js)
├── Regenerate indices
│     Scans docs/proposals/ and docs/architecture/adr/,
│     produces README.md index tables (replaces gen-index.py)
├── Health checks
│     ├── Validate tech-radar.yml (YAML syntax, required fields)
│     ├── Check broken ADR references in tech-radar.yml
│     ├── Check broken proposal references in tech-radar.yml
│     ├── Stale proposals (>6 months, no status changes)
│     ├── Orphaned adopt entries (ring: adopt with no links.adr)
│     └── Missing descriptions (<15 chars)
└── Browse
      ├── Browse ADRs (fuzzy filter → glow)
      ├── Browse proposals (fuzzy filter → glow)
      └── Browse tech radar (fuzzy filter, show ring/quadrant/why)
```

### File structure

```
github.com/pavlos/dotops/
├── flake.nix               # package derivation
├── flake.lock
├── bb.edn                  # deps: bblgum
├── dotops.bb               # entry point (Gum menu)
├── src/
│   ├── radar.bb            # YAML → radar HTML
│   ├── index.bb            # ADR/proposal index generation
│   ├── health.bb           # validators
│   └── browse.bb           # glow + fuzzy filter
└── .gitignore
```

### Dotfiles repo changes

```nix
# ~/.config/home-manager/flake.nix
inputs.dotops.url = "github:pavlos/dotops";

# ~/.config/home-manager/home.nix
home.packages = [
  inputs.dotops.packages.${pkgs.system}.default
  pkgs.glow
  pkgs.babashka
];
```

A single `.gitignore` is added to the dotfiles repo:

```
# docs/tech-radar/.gitignore
index.html
```

`docs/tech-radar.yml`, `docs/architecture/adr/`, and `docs/proposals/`
remain unchanged. `docs/proposals/gen-index.py` is archived once the
Babashka index generator proves reliable.

## Consequences

**Positive:**
- Single entry point (`dotops`) for all dotfile maintenance operations
- Interactive Gum menu with fuzzy filtering — no memorizing paths or
  flags
- Health checks catch broken cross-references, stale proposals, and
  undocumented technology choices
- Tech radar visualization generated from the existing YAML source of
  truth
- Separate repository respects dotfiles-as-declarative-config
  philosophy
- Version-locked via Nix flake input — reproducible builds

**Negative:**
- Requires network access to fetch the dotops flake input on first use
  (subsequent `home-manager switch` uses cached/nix-store)
- Adds babashka + glow + gum to the Nix closure (~200MB)
- Health checks produce output only; they do not auto-fix issues
- `bblgum` is a Babashka runtime dependency, resolved via `bb.edn`
  (requires network on first `bb` invocation, cached thereafter)

## Alternatives Considered

| Alternative | Reason Not Chosen |
|-------------|-------------------|
| Python script in dotfiles repo | Blurs declarative config vs imperative tooling; needs pyyaml dep |
| Embedded scripts in dotfiles repo (per-operation) | Scattered discoverability; no Gum menu; harder to keep DRY |
| AOE Technology Radar (TypeScript) | Heavier build chain; JS/TS dependency surface out of proportion for solo use |
| Bubble Tea (Go TUI framework) | Full application development required; Gum provides the interactive widgets without writing a custom TUI app |

## Status History

| Date       | Status     | Notes |
|------------|------------|-------|
| 2026-05-03 | Proposed   |       |
