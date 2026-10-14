---
title: "Modernize the Sway UI With a Shared Theme and Real Wallpapers"
date: 2026-10-14
status: Proposed
category: Sway
---

# PROP-0020: Modernize the Sway UI With a Shared Theme and Real Wallpapers

## Context

There is no shared color scheme across the desktop today — each component
hand-copies its own palette:

- **Sway** (`configs/sway/config:22-32`): `$cl_high #009ddc`, `$cl_indi`/
  `$cl_fore #d9d8d8`, `$cl_back #231f20`, `$cl_urge #ee2e24`.
- **Mako** (`configs/mako/config:12-18`): the sway palette, copy-pasted by
  hand.
- **Waybar** (`configs/waybar/style.css`): a third, dated look — flat,
  opaque `#222` background, `border-radius: 0` globally, a focused
  workspace color (`#c9545d`) that matches nothing else, raw CSS color
  names (`orange`, `yellow`, `#cc3300`) as separators, dead/unused
  selectors (e.g. `#custom-spotify` with no matching module).
- **Wofi** (`configs/wofi/style.css`): a fourth palette (Nord), the only
  component that's rounded/translucent.
- **Swaylock** (`configs/swaylock/config`): a fifth palette; not even
  deployed today (tracked by an existing TODO in `home.nix:449-450`, left
  untouched here).

Fonts referenced across configs (Noto Sans, Roboto, Inter, "FontAwesome 5
Free") mostly aren't installed — only DejaVu and Font Awesome **7** are
(`home.nix:284-290`). Cursor theme (Adwaita) is set imperatively three
different ways in the sway config (env exports that don't actually
propagate out of their `exec_always` subshell, `gsettings` calls, and
`seat * xcursor_theme`), instead of once, declaratively.

The wallpaper is a single stock `Sway_Wallpaper_Blue_1920x1080.png`
(`configs/sway/config:54`) applied via `output * bg ... fill` to every
output, including a rotated portrait monitor (Dell U2311H,
`outputs-home.conf`/`outputs-work.conf`, transform 270) that gets the same
landscape image center-cropped. The same wallpaper path is hardcoded a
second time in `.config/nixos/configuration.nix` for the greetd/ReGreet
login screen (blurred and dimmed via ImageMagick at build time), so
changing the wallpaper today means editing two unrelated files.

No ADR covers theming, and neither flake (`home-manager/flake.nix`,
`nixos/flake.nix`) references any theming framework (stylix, nix-colors,
base16, pywal, etc.).

This proposal does not change: swaylock theming (own TODO), the
`wlogout`/`wleave` inconsistency, wallpaper rotation/management tooling,
Viber notification styling (ADR-0008), or the Alacritty/foot terminal
palettes.

## Proposal

A small **home-grown theme module** — not stylix (too heavy/opinionated
for this repo's raw-dotfiles-via-`xdg.configFile` model), and not another
round of hand-copied hex values (how we got here). One Nix file defines
the palette, fonts, cursor, and wallpaper paths; both the home-manager
flake and the NixOS flake (for the greeter) import it, so there is exactly
one source of truth.

Palette: **Catppuccin Mocha**, blue accent (closest match to the existing
`#009ddc`). Wallpapers: nixpkgs' bundled
`pkgs.nixos-artwork.wallpapers.*` Catppuccin Mocha images — no image files
committed to the repo, and they match the palette automatically. (Two
personal photos found in `~/Pictures/wallpapers/` during investigation are
explicitly **not** included.)

### Palette

| Name | Hex | Catppuccin name | Used for |
|---|---|---|---|
| `bg` | `#1e1e2e` | base | windows, notifications, wofi |
| `bgAlt` | `#181825` | mantle | inactive window titles |
| `bgDark` | `#11111b` | crust | bar background, text-on-accent |
| `surface` | `#313244` | surface0 | hover, unfocused workspaces |
| `surfaceHi` | `#45475a` | surface1 | borders |
| `muted` | `#6c7086` | overlay0 | dimmed text |
| `fg` | `#cdd6f4` | text | main text |
| `fgDim` | `#a6adc8` | subtext0 | secondary text |
| `accent` | `#89b4fa` | blue | focus border/workspace, selection, mako border |
| `accentAlt` | `#b4befe` | lavender | clock, focus indicator |
| `urgent` | `#f38ba8` | red | urgent/critical states |
| `warning` | `#fab387` | peach | `#mode`, warnings |
| `caution` | `#f9e2af` | yellow | battery/temp warning |
| `ok` | `#a6e3a1` | green | charging, network up |
| `info` | `#94e2d5` | teal | tray/info |

Fonts: `Inter` (UI), `JetBrainsMono Nerd Font` (mono), `Font Awesome 7
Free` (icons — matching what's actually installed, not the "FontAwesome 5
Free" currently referenced). Cursor: `Bibata-Modern-Classic`, size 24.

### New files

- **`.config/home-manager/theme/default.nix`** — plain
  `{ pkgs }: { palette; fonts; cursor; wallpapers; }`, no home-manager/NixOS
  options, importable from both flakes.
  - `wallpapers.main` = `nixos-artwork.wallpapers.catppuccin-mocha`
    (landscape outputs)
  - `wallpapers.portrait` = the matching `nineish-catppuccin-mocha`
    variant (repeating pattern, crops cleanly on the rotated monitor)
  - `wallpapers.greeter` = the existing ImageMagick blur+dim step, moved
    here unchanged from `configuration.nix`, applied to `main`
- **`.config/home-manager/modules/theme.nix`** — home-manager module
  (same pattern as `modules/claude-code.nix`):
  - packages: `inter`, `nerd-fonts.jetbrains-mono`, `adw-gtk3`,
    `papirus-icon-theme`, `bibata-cursors` (and remove the stale,
    commented-out `nerdfonts.override` at `home.nix:84` — old API)
  - `home.pointerCursor` (`gtk.enable = true`)
  - `gtk.enable` with `adw-gtk3-dark` theme, `Papirus-Dark` icons, Inter
    font
  - `dconf.settings` → `prefer-dark` color-scheme
  - `qt.enable` with `platformTheme.name = "gtk3"`
  - `xdg.configFile."sway/theme.conf"` — `set $c_bg ...` etc., plus
    `$wallpaper_main`/`$wallpaper_portrait`, font, `seat * xcursor_theme`
  - `xdg.configFile."waybar/colors.css"` / `"wofi/colors.css"` —
    `@define-color` lines
  - `xdg.configFile."mako/config".source = pkgs.replaceVars
    ./../configs/mako/config.in { ... }`

### Edits

**`.config/home-manager/home.nix`**: import `./modules/theme.nix`; remove
the now-superseded mako `xdg.configFile` line; fix the stale azote comment
(it is in nixpkgs, just intentionally unused — it writes an imperative
`~/.azotebg` that would conflict with declaring wallpapers in Nix).

**`.config/home-manager/configs/sway/config`**:
- `include ~/.config/sway/theme.conf` near the top.
- Replace the `$cl_*` variables and `client.*` lines with the new `$c_*`
  palette variables; add `client.background $c_bg`.
- `output * bg $wallpaper_main fill`; add `output $monitor3 bg
  $wallpaper_portrait fill` after the monitor variables are defined
  (before the per-profile `include outputs-...`).
- Delete the imperative cursor lines (the `exec_always export XCURSOR_*`
  lines that never actually propagated out of their subshell, the
  `gsettings` calls, and `seat * xcursor_theme`) — now handled
  declaratively.

**`.config/home-manager/configs/waybar/style.css`** (rewrite): import
`colors.css`; switch fonts to Inter/JetBrainsMono Nerd/Font Awesome 7;
remove the global `border-radius: 0`; transparent bar background with
floating rounded "pill" module groups; recolor workspaces/`#mode`/
battery/temperature states from the new palette; delete dead CSS
selectors (cross-checked against the actual module list first).

**`.config/home-manager/configs/wofi/style.css` + `config`**: swap Nord
colors for the shared palette via `@import "colors.css"`; switch font to
Inter; keep the existing rounded/translucent look; fix the stale
`term=gnome-terminal` → `term=foot`; delete the stray
`style.ccs_nocolors` backup file.

**`.config/home-manager/configs/mako/config` → `config.in`**: convert
hardcoded colors to `@bg@`/`@fg@`/`@accent@`/`@urgent@` placeholders
filled by `replaceVars`; update font/border/radius/padding to match.

**`.config/nixos/configuration.nix`**: `let theme = import
../home-manager/theme { inherit pkgs; };`; delete the local
`greeterWallpaper`/`greeterWallpaperFaded` definitions in favor of
`theme.wallpapers.greeter`; add `programs.dconf.enable = true;` (required
for home-manager's `dconf.settings`, not currently enabled). This file
has other unrelated uncommitted changes in the working tree — keep this
edit separable so it can be committed on its own.

Add a new ADR, `docs/architecture/adr/0014-ui-theming-shared-palette.md`,
documenting the theme-module decision, the Catppuccin Mocha choice, and
why stylix was rejected; add a one-line pointer in `CLAUDE.md`.

## Tasks

- [ ] Create `theme/default.nix` and `modules/theme.nix`
- [ ] Wire the module into `home.nix`
- [ ] Rewrite sway's color/wallpaper/cursor sections
- [ ] Convert mako to a `replaceVars` template
- [ ] Rewrite waybar's `style.css` and fix its module/CSS mismatches
- [ ] Rewrite wofi's `style.css`/`config`, delete the stray backup file
- [ ] Point `configuration.nix`'s greeter at `theme.wallpapers.greeter`;
      enable `programs.dconf`
- [ ] Write ADR-0014; add a `CLAUDE.md` pointer
- [ ] Verify per the steps below

## Verification

1. `home-manager build --flake .#pmelissinos` then `switch` (from
   `.config/home-manager`); `sudo nixos-rebuild build --flake
   .config/nixos#localhost-nixos` then `switch` (both flakes import
   `home.nix`, so both must evaluate cleanly).
2. `cat ~/.config/sway/theme.conf ~/.config/waybar/colors.css
   ~/.config/mako/config` — real hex values, no leftover `@x@`
   placeholders.
3. `swaymsg reload` — no error bar; focused borders blue; title font
   Inter.
4. `pkill waybar; waybar -l debug 2>&1 | grep -iE 'css|error'` — no CSS
   errors; visually confirm rounded pills, blue focused workspace, icons
   rendering (not boxes), `$mod+r` turns `#mode` peach.
5. `wofi --show drun` — rounded/translucent, blue selection, Inter font.
6. `makoctl reload; notify-send Test normal; notify-send -u critical Test
   urgent` — rounded corners, blue border normal / red border critical.
7. Cursor/GTK: same cursor across foot/Firefox/GTK3/GTK4 apps;
   `gsettings get org.gnome.desktop.interface color-scheme` →
   `'prefer-dark'`.
8. Wallpapers: check every landscape output plus the rotated Dell U2311H
   (no stretch/crop artifacts); log out and confirm ReGreet shows the
   blurred Catppuccin wallpaper.
9. `rg -i '009ddc|231f20|d9d8d8|ee2e24|c9545d|64727d|5e81ac|FontAwesome 5|Noto Sans|Roboto' .config/home-manager/configs`
   — no matches outside swaylock/alacritty (intentionally untouched).

## Consequences

**Positive:**
- One source of truth for colors/fonts/cursor/wallpaper, shared by the
  desktop and the login screen — no more copy-pasted hex values drifting
  apart.
- Visually consistent, modern look across sway/waybar/wofi/mako.
- Fixes font references that were silently falling back to DejaVu.
- Cursor theme set declaratively once instead of three imperative,
  partially-broken ways.
- Per-output wallpaper handling fixes the stretched portrait monitor.

**Negative:**
- Adds several new font/theme/icon packages to the closure (modest size).
- Mako's config becomes a generated file (`replaceVars` template) rather
  than a plain static file — slightly less trivial to hand-edit directly,
  though still just editing `config.in`.
- Touches many files in one pass; should be applied/tested incrementally
  (sway → mako → waybar → wofi → greeter) rather than all at once.

## Alternatives Considered

| Alternative | Reason Not Chosen |
|-------------|-------------------|
| Adopt stylix | Wants to own `programs.*` for waybar/mako/GTK/Qt/firefox/emacs via home-manager options, which conflicts with this repo's raw-dotfiles-via-`xdg.configFile` model; too much for five configs |
| Hand-copy one consistent set of hex values into each config directly, no shared module | Exactly how the repo ended up with 4-5 divergent palettes already; no single source of truth, greeter stays out of sync |
| Include the 2 personal photos from `~/Pictures/wallpapers/` | Not appropriate to commit personal images without explicit request; nixos-artwork's Catppuccin set already matches the palette with zero repo bloat |

## Status History

| Date       | Status     | Notes |
|------------|------------|-------|
| 2026-10-14 | Proposed   | Catppuccin Mocha + home-grown theme module + nixos-artwork wallpapers confirmed with user |
