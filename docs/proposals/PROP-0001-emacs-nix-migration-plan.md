---
title: "Emacs Configuration Modernization"
date: 2026-04-19
status: In Progress
category: Emacs
---

# PROP-0001: Emacs Configuration Modernization

## Goal

Fix several low-risk bugs and replace unmaintained or redundant packages
in the Emacs configuration. No architectural changes — just targeted
cleanup that improves correctness and removes dead weight.

## Current State

- Emacs (pgtk) installed via home-manager (`programs.emacs`)
- 5 packages in `extraPackages`: `eat`, `magit`, `nix-mode`, `org-roam`, `vterm`
- Remaining ~65 packages installed via `package.el` (`:ensure t` in `init.el`)
- Several known issues: typo in dap keybinding, unmaintained
  `highlight-symbol`, redundant `neotree`, no-op `exec-path-from-shell`,
  outdated `pixel-scroll-mode`

## Changes

### Remove use-package bootstrap

Built into Emacs 29+. Remove the guard at lines 92-96 that installs
`use-package` via package.el. Keep `(eval-when-compile (require 'use-package))`
and `(require 'bind-key)`.

### Fix dap-step-out typo

Line 1428: `dsp-step-out` → `dap-step-out`

### Remove exec-path-from-shell

No-op on this system (guarded by `(memq window-system '(mac ns x))`).
Home-manager handles PATH via session variables. Simplify `safe-getenv`
to plain `getenv`.

### Remove neotree

Redundant with treemacs (which has full git/projectile integration). Also
remove `(doom-themes-neotree-config)` call.

### Switch pixel-scroll-mode → pixel-scroll-precision-mode

Built into Emacs 29+. Replace `(pixel-scroll-mode)` with
`(pixel-scroll-precision-mode)`. Remove manual `pixel-dead-time` and
`pixel-resolution-fine-flag` settings (handled by the new mode).

### Replace highlight-symbol → symbol-overlay

highlight-symbol is unmaintained (repo archived). symbol-overlay is a
drop-in replacement with identical keybindings (`C-,` / `C-.`).

## Tasks

- [ ] Remove `use-package` bootstrap guard (lines 92-96)
- [ ] Fix `dsp-step-out` → `dap-step-out` typo (line 1428)
- [ ] Remove `exec-path-from-shell` block, simplify `safe-getenv`
- [ ] Remove `neotree` use-package block + `doom-themes-neotree-config` call
- [ ] Switch `pixel-scroll-mode` → `pixel-scroll-precision-mode`
- [ ] Replace `highlight-symbol` → `symbol-overlay` (add to home.nix)
- [ ] `home-manager switch --flake .` and verify Emacs loads cleanly

## Risk assessment

- **Low risk**: All changes are targeted and individually revertible
- **symbol-overlay**: Must be added to `extraPackages` in home.nix
- **Rollback**: `git revert` or `home-manager switch` to previous generation

## Status History

| Date       | Status      | Notes |
|------------|-------------|-------|
| 2026-04-19 | Proposed    | Original migration + modernization proposal |
| 2026-06-06 | In Progress | Task checklist added; scope split — migration moved to PROP-0017 |
