---
title: "Emacs TypeScript/JavaScript LSP Support via typescript-language-server"
date: 2026-04-29
status: Proposed
category: Emacs
---

# Emacs TypeScript/JavaScript LSP Support via typescript-language-server

## Goal

Enable full Language Server Protocol (LSP) support for TypeScript, JavaScript, JSX, and TSX files in Emacs. This provides IntelliSense-quality features — completions, go-to-definition, inline type hints, hover docs, refactoring — powered by the same TypeScript engine that drives VSCode.

## Context

### Current State

Emacs currently has no TypeScript or JavaScript language server integration. Opening a `.ts` or `.js` file relies on Emacs built-in `typescript-mode` and `javascript-mode`, which provide only basic font-lock syntax highlighting.

The following infrastructure already exists:

| Component | Status |
|---|---|
| `lsp-mode` (Emacs LSP client) | Installed, configured |
| `nodejs_22` (via home-manager) | Installed |
| `typescript-language-server` (via home-manager) | Installed in nix, not yet wired to Emacs |
| `libvterm` (via home-manager) | Installed, resolves vterm dependency |
| Tree-sitter grammar for TS | **Missing** — not available on Nix, causes errors |

### Problems

1. **No LSP for TypeScript/JavaScript** — completions, go-to-definition, hover docs, and inline type hints are unavailable.
2. **Tree-sitter failure on startup** — Emacs 30 attempts to load `libtree-sitter-typescript.so` for native TS parsing, but this shared object is unavailable in the Nix environment. Every `.ts` file load produces a cascade of `file-missing` errors:
   ```
   Warning (treesit): Cannot activate tree-sitter, because language grammar
   for typescript is unavailable: ... libtree-sitter-typescript.so ...
   ```
3. **vterm "compile now?" prompt** — at startup, vterm checks for its native module and prompts interactively when not found. With `libvterm` now available via Nix, this should compile cleanly but the deferral is missing.

## Proposal

### Approach

Use **`typescript-language-server`** (ts-ls) as the language server. This is a thin LSP wrapper around Microsoft's `tsserver` — the same TypeScript engine that powers VSCode. It handles TypeScript, JavaScript, JSX, and TSX with full fidelity.

This is the **recommended** approach per the [lsp-mode documentation](https://emacs-lsp.github.io/lsp-mode/page/lsp-typescript/) and is maintained by the TypeFox community with active Emacs integration.

### Alternatives Considered

#### vtsls (Vue/VSCodium TypeScript Language Server)
- More recent fork closer to VSCode's actual TS integration
- Faster project initialization
- **Rejected**: Less Emacs-specific documentation, smaller community, more complexity for marginal gain

#### deno (Deno's built-in LSP)
- **Rejected**: Requires Deno runtime, different project model, not suitable for mixed TS/JS codebases

#### tsserver via tide (deprecated)
- **Rejected**: tide is unmaintained, tsserver direct integration is obsolete

### Implementation

#### Phase 1: Fix tree-sitter fallback

**File**: `init.el`

Disable tree-sitter grammar loading for TypeScript and TSX, allowing Emacs to fall back to font-lock + LSP semantic highlighting:

```elisp
;; Placed before package initialization (~line 15)
(setq treesit-language-source-alist
      (assq-delete-all 'typescript treesit-language-source-alist)
      treesit-language-source-alist
      (assq-delete-all 'tsx treesit-language-source-alist))
```

This is a **runtime fallback** — it does not install anything, only redirects Emacs away from the unavailable native grammar.

#### Phase 2: Wire ts-ls into lsp-mode

**File**: `init.el`

Add `lsp-deferred` hooks for TypeScript and JavaScript major modes in the existing `lsp-mode` use-package block:

```elisp
:hook ((python-mode . lsp-deferred)
       (typescript-mode . lsp-deferred)
       (tsx-tsx-mode . lsp-deferred)
       (javascript-mode . lsp-deferred)
       (js-jsx-mode . lsp-deferred)
       (java-mode . lsp)
       ...)
```

The `typescript-language-server` binary in PATH (via Nix) is auto-detected by lsp-mode's built-in `ts-ls` client.

#### Phase 3: Add lsp-javascript and lsp-typescript extension packages

**File**: `init.el`

These packages expose ~100 combined customization variables for TS/JS-specific LSP features (inlay hints, format settings, import organization, etc.). They add no runtime overhead — only extra configuration options.

```elisp
(use-package lsp-javascript
  :ensure t
  :after lsp-mode)

(use-package lsp-typescript
  :ensure t
  :after lsp-mode)
```

#### Phase 4: Defer vterm loading

**File**: `init.el`

Change the vterm use-package block to defer loading, preventing the `compile now?` prompt at startup:

```elisp
(use-package vterm
  :ensure t
  :defer t)   ;; was: no :defer, loads at startup
```

When `M-x vterm` is invoked, vterm checks for `libvterm.so` (now available via Nix) and compiles its native module silently if needed.

#### Phase 5: Enable NonGNU ELPA archive

**File**: `init.el`

The `eat` terminal emulator package is hosted on NonGNU ELPA, which is currently commented out in `package-archives`:

```elisp
(setq package-archives
      '(("GNU ELPA"    . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")
        ("NonGNU ELPA"  . "https://elpa.nongnu.org/nongnu/")))
```

This resolves the `Package 'eat' is unavailable` error.

### Files Affected

| File | Changes |
|---|---|
| `~/.config/home-manager/configs/emacs/init.el` | Phase 1–4: tree-sitter fallback, lsp hooks, extension packages, vterm deferral |
| `~/.config/home-manager/home.nix` | **No changes** — `typescript-language-server` (line 77) and `libvterm` (line 72) already present |

### Home-manager Rebuild Required

```bash
cd ~/.config/home-manager && home-manager switch --flake .
```

Then restart Emacs.

## Consequences

### Expected Outcomes

- `.ts`, `.tsx`, `.js`, `.jsx` files open with full LSP support
- Completions, go-to-definition, hover docs, inline type hints, refactoring all work
- No more tree-sitter `file-missing` errors on TypeScript files
- No more vterm "compile now?" prompt on startup
- `eat` package available from NonGNU ELPA

### Risks

- **Nix PATH**: `typescript-language-server` must be findable in `exec-path`. With home-manager managed PATH, this should work automatically, but if Emacs is launched outside the home-manager environment, the binary may not be found.
- **Large projects**: The tsserver process can consume significant memory. `lsp-mode` handles this, but very large monorepos may need `lsp-mode` memory tuning (already configured with `gc-cons-threshold 1600000`).

### Rollback

All changes are additive and non-destructive. To rollback:
1. Remove the `typescript-mode`/`javascript-mode` entries from the lsp-mode `:hook`
2. Remove the `lsp-javascript`/`lsp-typescript` use-package blocks
3. Remove the `treesit-language-source-alist` modification
4. Re-add `NonGNU ELPA` to `package-archives` comment if unwanted
5. Rebuild with `home-manager switch --flake .`

## Verification

After rebuilding and restarting Emacs:

1. Open a `.ts` file — `LSP` should appear in the mode line within seconds
2. Run `M-x lsp-workspace-show-trace` to confirm ts-ls connected
3. Test `M-.` (xref-find-definitions) on a type or function
4. Confirm no tree-sitter warnings in `*Messages*`

## Related Issues

This work resolves multiple errors visible in the Emacs startup Messages buffer:

- `Warning (treesit): Cannot activate tree-sitter... libtree-sitter-typescript`
- `⛔ Error (use-package): Cannot load eat`
- `⛔ Error (use-package): Cannot load vterm` (vterm's compile prompt)

## Todo

- [ ] Apply tree-sitter fallback to `init.el`
- [ ] Extend `lsp-mode` `:hook` with TS/JS modes
- [ ] Add `lsp-javascript` and `lsp-typescript` use-package blocks
- [ ] Defer vterm loading
- [ ] Enable NonGNU ELPA archive
- [ ] Run `home-manager switch --flake .`
- [ ] Verify LSP connects for TypeScript files
- [ ] Confirm no tree-sitter errors in Messages buffer
