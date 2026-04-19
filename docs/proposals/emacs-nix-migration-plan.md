# Emacs Package Management Migration: package.el → home-manager/nix

**Date**: 2026-04-19
**Status**: Proposed
**Migration Type**: Emacs package.el → home-manager with emacs-overlay

## Goal

Unify the package update workflow so that `nix flake update && home-manager
switch` updates system packages, desktop applications, AND Emacs packages in a
single command. Eliminate the separate `M-x package-list-packages U x` workflow.

## Current State

- Emacs (pgtk) installed via home-manager (`programs.emacs`)
- Only 2 packages in `extraPackages`: `nix-mode`, `magit`
- ~67 packages installed via `package.el` (`:ensure t` in `init.el`)
- `tiling` already vendored locally under `~/.config/emacs/lisp/`
- `package.el` initialization boilerplate in `init.el` (lines 74-103)

## Changes

### Phase 1: Package management unification

#### Step 1 — Add emacs-overlay as flake input

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

#### Step 2 — Expand extraPackages in home.nix

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

#### Step 3 — Remove :ensure t from all use-package blocks in init.el

Remove `:ensure t` from every `use-package` declaration (~67 occurrences).
Packages will be pre-installed by nix.

#### Step 4 — Remove package.el initialization from init.el

Remove/disable the package.el boilerplate (lines 74-103):
- `setq package-archives ...`
- `package-initialize`
- The `use-package` bootstrap block (install use-package if missing)
- The `package-refresh-contents` fallback

#### Step 5 — Apply and test

```bash
cd ~/.config/home-manager
nix flake update
home-manager switch --flake .
```

Restart Emacs and verify all packages load correctly.

#### Step 6 — Clean up old package.el state

After confirming everything works:

```bash
rm -rf ~/.cache/emacs/elpa
```

### Phase 2: Trivial modernizations (bundle with Phase 1)

These are low-risk changes that should be done alongside the migration:

#### Remove use-package bootstrap

Built into Emacs 29+. Remove the guard at lines 92-99 that installs
`use-package` via package.el. Keep `(eval-when-compile (require 'use-package))`
and `(require 'bind-key)`.

#### Fix dap-step-out typo

Line 1427: `dsp-step-out` → `dap-step-out`

#### Remove exec-path-from-shell

No-op on this system (guarded by `(memq window-system '(mac ns x))`).
Home-manager handles PATH via session variables. Simplify `safe-getenv` to
plain `getenv`.

#### Remove neotree

Redundant with treemacs (which has full git/projectile integration). Also
remove `(doom-themes-neotree-config)` call.

#### Switch pixel-scroll-mode → pixel-scroll-precision-mode

Built into Emacs 29+. Replace `(pixel-scroll-mode)` with
`(pixel-scroll-precision-mode)`. Remove manual `pixel-dead-time` and
`pixel-resolution-fine-flag` settings (handled by the new mode).

#### Replace highlight-symbol → symbol-overlay

highlight-symbol is unmaintained (repo archived). symbol-overlay is a
drop-in replacement with identical keybindings (`C-,` / `C-.`).

### Phase 3: Medium modernizations (optional, separate session)

#### Replace company → corfu

Modern completion UI. Uses child frames (native on PGTK), works with
`completion-at-point-functions` natively. ~15 lines config vs ~40 lines.
`cape-company-to-capf` adapter available for migration.

#### Replace undo-tree → built-in undo + vundo

undo-tree has known corruption issues. Emacs 29+ built-in undo is solid.
`vundo` provides a visual tree viewer without managing its own state.

### Phase 4: Large modernizations (future, separate sessions)

#### Replace ivy/counsel/swiper → vertico/consult/orderless/marginalia

Modern community standard. Lighter, better integration with Emacs completion
API. Requires updating projectile config, hydra references, many `counsel-*`
calls. Deserves its own focused session.

#### Evaluate projectile vs project.el

project.el has caught up significantly in Emacs 29/30, but projectile still
offers `clojure-jump-to-test` and project-wide replace. Defer for now.

## Future workflow

```bash
# Update everything (system + desktop + emacs):
cd ~/.config/home-manager && nix flake update && home-manager switch --flake .

# Add a new emacs package:
# 1. Add epkgs.<package> to extraPackages in home.nix
# 2. Add use-package block in init.el (without :ensure t)
# 3. home-manager switch --flake .
```

## Risk assessment

- **emacs-overlay build failures**: If an overlay package breaks, pin with
  `nix flake lock --update-input emacs-overlay` or fall back to nixpkgs
- **Missing packages**: All 65 verified available. `tiling` already vendored
  locally
- **Rollback**: `home-manager generations` provides instant rollback if
  anything breaks
