# ADR-0013: Claude Code config as a writable, captured file tree

**Date**: 2026-09-29
**Status**: Accepted

## Context

Claude Code was installed as an unmanaged package
(`home.packages = [ "claude-code" ... ]`) and wrote its state to the
upstream default, `~/.claude`, plus `~/.claude.json` directly in `$HOME`.
Neither location was declared in home-manager, which meant:

- `settings.json` (model, theme, TUI mode) was edited in place and never
  captured in the repo.
- Claude's config lived outside `$XDG_CONFIG_HOME`, unlike every other
  managed program (see [ADR-0003](0003-xdg-directory-compliance.md)).

home-manager's `programs.claude-code` module (nixpkgs unstable /
home-manager `release-26.05`) has a `configDir` option: pointed anywhere
other than `~/.claude`, it exports `CLAUDE_CONFIG_DIR` for the CLI. It also
has a `settings` option that deploys `settings.json` as a **read-only
symlink into the Nix store**, generated from Nix values.

That symlink approach was tried first and rejected: Claude Code writes its
own settings.json at runtime (`/model`, `/config`, plugins, effort level),
and a store symlink can't be written to. The in-app change either fails
outright or silently doesn't persist. It also has no facility to capture
config-directory changes back into a Nix expression — Nix is one-way by
design.

## Decision

Use `configDir` for the XDG location, but not `settings`. Instead,
`.config/home-manager/modules/claude-code.nix` deploys
`configs/claude/` as **writable, ordinary files** and syncs them with
Nix-native `home.activation` plus two helper commands — no store symlink,
no translation into Nix syntax, ever.

```nix
programs.claude-code = {
  enable = true;
  configDir = "${config.xdg.configHome}/claude";  # settings left unset
};
```

**Tracked** (`configs/claude/`, in the repo): `settings.json`,
`keybindings.json`, `CLAUDE.md`, `agents/`, `commands/`, `output-styles/`,
`hooks/`, `skills/` (excluding `skills/synced/`, which the CLI downloads on
its own).

**Left alone** (Claude-managed state and cache, never touched by this
mechanism): `.credentials.json`, `.claude.json`, `projects/` (transcripts
and auto-memory), `history.jsonl`, `sessions/`, `session-env/`,
`shell-snapshots/`, `paste-cache/`, `file-history/`, `plans/`, `plugins/`,
`skills/synced/`, `cache/`, `backups/`, `stats-cache.json`,
`policy-limits*`, `remote-settings.json`, `.last-cleanup`.

### The sync rule

`home.activation.claudeConfig` compares, file by file, the live copy under
`~/.config/claude` against a hash recorded at
`$XDG_STATE_HOME/claude-config/deployed/<path>.sha256` — the hash the live
file had the last time this mechanism deployed or captured it:

| Live file vs. recorded hash | Repo changed? | Action |
|---|---|---|
| Missing | any | Deploy from the repo |
| Symlink into `/nix/store` (an earlier generation's artifact) | any | Replace, then deploy from the repo |
| Matches the record (no unsaved edits) | yes | Deploy from the repo |
| Matches the record | no | Do nothing |
| Differs from the record (unsaved edits) | any | **Never overwrite.** Warn |

`claude-config-check` runs the same comparison without writing anything,
and sends a desktop notification (`notify-send`, via `libnotify`) when
something is uncaptured. `systemd.user.timers.claude-config-check` runs it
daily.

`claude-config-capture` is the only thing that pulls a live edit back into
the repo. For each file that differs, it shows a diff and asks
`[y]es/[N]o/[q]uit`. On yes, it copies the live file into the repo's
working tree and updates the recorded hash. It never runs `git commit` —
you review the result with `git diff` and commit it yourself.

## Consequences

### Positive
- ✅ **No round-trip UX cost**: `/model`, `/config`, and friends apply
  immediately, exactly like an unmanaged install.
- ✅ **One source of truth, by choice, not by force**: a live edit only
  reaches the repo when `claude-config-capture` is run and you say yes to
  it. `git diff`/`git status` show drift explicitly; nothing is silently
  out of sync.
- ✅ **Portable**: a new machine needs the repo cloned to
  `~/workspace/dotfiles` and `home-manager switch --flake .` — the tracked
  files (settings, and later agents/commands/skills) deploy immediately.
- ✅ **Never clobbers unsaved work**: the hash-record check means a switch
  can never silently discard an in-app change you haven't captured yet.

### Negative
- ⚠️ **Not an upstream feature.** This is ~180 lines of custom Nix/bash
  in `modules/claude-code.nix`, not something home-manager provides. It
  needs the same care as any other hand-rolled sync tool.
- ⚠️ **Hardcoded repo path.** `claude-config-capture`/`check` assume the
  repo lives at `~/workspace/dotfiles`. A clone anywhere else needs the
  path in `claudeDotfilesRepo` updated.
- ⚠️ **Deleting a tracked key needs a capture.** Removing something from
  `configs/claude/settings.json` by hand in the repo, without also editing
  or removing it live, leaves the live copy holding the old value
  (deploy only ever overwrites existing keys via a whole-file copy, not a
  merge — but the live *file* is what a capture would show as changed, so
  a hand-edit to the repo alone is fine; a value removed only live and not
  captured is what gets left behind).
- ⚠️ **Not fully XDG-pure**: OAuth credentials, history, and transcripts
  end up under `~/.config/claude` rather than `$XDG_STATE_HOME` or
  `$XDG_DATA_HOME`, because Claude Code has only one config directory to
  point at.
- ⚠️ **Never put secrets in a tracked file.** `configs/claude/` is
  committed to the repo; credentials live in `.credentials.json`/
  `.claude.json`, which this mechanism does not touch, and MCP server
  configs with embedded secrets should stay untracked too.

## Migration notes

- `CLAUDE_CONFIG_DIR` is exported by `hm-session-vars.sh`, which only
  takes effect in a new session (a fresh terminal is often enough if it
  sources it; a full sway logout/login guarantees it everywhere).
- Moving the *old* `~/.claude` state (transcripts, credentials, history)
  into `~/.config/claude` is a separate, one-time step from enabling this
  module, and should only be done with every Claude Code session quit
  first — once any session is actively using `~/.config/claude`, copying
  old state on top of it risks clobbering newer live data (this was
  observed in practice: a second terminal picked up `CLAUDE_CONFIG_DIR`
  and began writing real project transcripts under
  `~/.config/claude/projects/` before the old-state migration ran).

## References

- [ADR-0003: XDG Base Directory Specification Compliance][adr0003]
- `.config/home-manager/modules/claude-code.nix`
- home-manager `programs.claude-code` module:
  `modules/programs/claude-code.nix` (home-manager `release-26.05`)

[adr0003]: 0003-xdg-directory-compliance.md
