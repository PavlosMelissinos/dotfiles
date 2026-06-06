---
title: "Emacs Package Management Migration: package.el → home-manager/nix"
date: 2026-06-06
status: Proposed
category: Emacs
---

# PROP-0017: Emacs Package Management Migration: package.el → home-manager/nix

## Context

Emacs currently has ~65 packages installed via `package.el`
(`:ensure t` in `init.el`), with only 5 managed declaratively through
`programs.emacs.extraPackages` in home.nix. Package versions drift
independently from the nixpkgs pinning, breaking reproducibility.

The original motivation appeared in PROP-0001 (April 2026). The
modernization tasks from that proposal were extracted and implemented
separately, leaving only the package management unification for this
proposal.

## Proposal

Unify the package update workflow so that `nix flake update && home-manager
switch` updates system packages, desktop applications, AND Emacs packages in
a single command. Eliminate the separate `M-x package-list-packages U x`
workflow.

### Step 1 — Add emacs-overlay as flake input

Add `emacs-overlay` to `flake.nix` inputs and apply it as an overlay in
`home-manager.lib.homeManagerConfiguration`. This provides fresh MELPA/ELPA
packages and solves the nixpkgs update lag.

```nix
# flake.nix
inputs = {
  nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
  emacs-overlay.url = "github:nix-community/emacs-overlay";
  ...
};

outputs = { nixpkgs, emacs-overlay, ... }:
  let
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    homeConfigurations."pavlos" = home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = [{
        nixpkgs.overlays = [ emacs-overlay.overlays.default ];
      } ./home.nix];
    };
  };
```

### Step 2 — Expand extraPackages in home.nix

Replace the current `extraPackages` with all 65 packages. All verified
available in nixpkgs:

```nix
programs.emacs = {
  enable = true;
  package = pkgs.emacs-pgtk;
  extraPackages = epkgs: [
    epkgs.all-the-icons
    epkgs.blamer
    epkgs.bm
    epkgs.browse-at-remote
    epkgs.browse-kill-ring
    epkgs.cider
    epkgs.clj-refactor
    epkgs.clojure-mode
    epkgs.clojure-snippets
    epkgs.company
    epkgs.counsel
    epkgs.crux
    epkgs.dap-mode
    epkgs.deadgrep
    epkgs.diminish
    epkgs.docker-compose-mode
    epkgs.dockerfile-mode
    epkgs.doom-themes
    epkgs.eat
    epkgs.ess
    epkgs.exec-path-from-shell
    epkgs.expand-region
    epkgs.flycheck
    epkgs.flycheck-clj-kondo
    epkgs.forge
    epkgs.gradle-mode
    epkgs.graphviz-dot-mode
    epkgs.hide-mode-line
    epkgs.highlight-symbol
    epkgs.hydra
    epkgs.json-mode
    epkgs.keycast
    epkgs.lsp-java
    epkgs.lsp-mode
    epkgs.lsp-pyright
    epkgs.lsp-ui
    epkgs.magit
    epkgs.markdown-mode
    epkgs.maven-test-mode
    epkgs.multiple-cursors
    epkgs.neotree
    epkgs.nix-mode
    epkgs.numpydoc
    epkgs.org-jira
    epkgs.org-roam
    epkgs.org-superstar
    epkgs.org-tree-slide
    epkgs.paredit
    epkgs.paren-face
    epkgs.projectile
    epkgs.python-black
    epkgs.python-pytest
    epkgs.restclient
    epkgs.swiper
    epkgs.terraform-mode
    epkgs.treemacs
    epkgs.treemacs-magit
    epkgs.treemacs-projectile
    epkgs.undo-tree
    epkgs.use-package
    epkgs.vterm
    epkgs.which-key
    epkgs.with-editor
    epkgs.yaml-mode
    epkgs.yasnippet
    epkgs.yasnippet-snippets
  ];
};
```

### Step 3 — Remove :ensure t from all use-package blocks

Remove `:ensure t` from every `use-package` declaration (~67 occurrences).
Packages will be pre-installed by nix.

### Step 4 — Remove package.el initialization

Remove the package.el boilerplate in `init.el`:
- `setq package-archives ...`
- `package-initialize`
- The `use-package` bootstrap block (install use-package if missing)
- The `package-refresh-contents` fallback

Keep `(eval-when-compile (require 'use-package))` and
`(require 'bind-key)`.

### Step 5 — Apply and test

```bash
cd ~/.config/home-manager
nix flake update
home-manager switch --flake .
```

Restart Emacs and verify all packages load correctly.

### Step 6 — Clean up old package.el state

```bash
rm -rf ~/.cache/emacs/elpa
```

## Tasks

- [ ] Add emacs-overlay flake input
- [ ] Expand extraPackages in home.nix (65 packages)
- [ ] Remove `:ensure t` from all `use-package` blocks (~67 occurrences)
- [ ] Remove package.el initialization boilerplate
- [ ] `home-manager switch --flake .` and verify Emacs loads
- [ ] Clean up `~/.cache/emacs/elpa`

## Future workflow

```bash
# Update everything (system + desktop + emacs):
cd ~/.config/home-manager && nix flake update && home-manager switch --flake .

# Add a new emacs package:
# 1. Add epkgs.<package> to extraPackages in home.nix
# 2. Add use-package block in init.el (without :ensure t)
# 3. home-manager switch --flake .
```

## Consequences

**Positive:**
- Single update command for all packages
- Reproducible Emacs package versions via nixpkgs pinning
- Faster startup (no `package-refresh-contents` on fresh install)

**Negative:**
- Adds emacs-overlay as a flake dependency
- emacs-overlay build failures would block `home-manager switch`
- 65-package migration is high-effort with many touch points

## Risk assessment

- **emacs-overlay build failures**: If an overlay package breaks, pin with
  `nix flake lock --update-input emacs-overlay` or fall back to nixpkgs
- **Missing packages**: All 65 verified available. `tiling` already vendored
  locally
- **Rollback**: `home-manager generations` provides instant rollback if
  anything breaks

## Status History

| Date       | Status   | Notes |
|------------|----------|-------|
| 2026-06-06 | Proposed | Extracted from PROP-0001; migration deferred until modernization completed |
