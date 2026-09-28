# ADR-0012: Native Viber Package and nixGL Removal on NixOS

**Date**: 2026-09-28
**Status**: Accepted
**Supersedes**: ADR-0011 (Viber nixGL Integration and Dynamic Icon Extraction),
ADR-0008 (Viber AppImage Packaging Solution)

## Context

ADR-0008 and ADR-0011 solved Viber by wrapping the upstream AppImage with
nixGL. Those decisions were made while the machine ran a foreign distribution
(Fedora), where the nixpkgs `viber` package was broken and nixGL was required
to bridge Nix-built applications to the host graphics stack.

The machine now runs NixOS 26.05. Two things changed:

- **The upstream package was fixed.** `pkgs.viber` (24.9.0.3) now links
  `libxml2.so.2` explicitly (nixpkgs PR #396195) and ships its own desktop
  entry and brand icon. It is no longer marked broken.
- **nixGL is obsolete.** NixOS provides the graphics stack directly, so the
  nixGL overlay, the `nixGLIntel` wrappers around other GUI apps, and the
  `targets.genericLinux` shim are unnecessary.

The AppImage approach also depended on a manually placed file at
`~/.local/bin/viber.AppImage`. That file was never managed declaratively and
had disappeared, so `viber` failed with:

```
/home/pmelissinos/.nix-profile/bin/viber: line 5:
  /home/pmelissinos/.local/bin/viber.AppImage: No such file or directory
```

## Decision

Use the native `pkgs.viber` package and remove all nixGL infrastructure.

### Viber
1. Replace the custom `writeShellScriptBin "viber"`, the `xdg-open-firefox`
   helper, the custom `.desktop` file, and the AppImage icon extraction with
   the single `viber` package.
2. Add `"viber"` to `nixpkgs.config.allowUnfreePredicate` so the standalone
   home-manager flake evaluates the unfree package.
3. Drop the now-unused `appimage-run` package.

### nixGL
4. Unwrap `publii` and `steam` (remove `nixGLIntel`).
5. Remove `targets.genericLinux.enable` and
   `nixpkgs.overlays = [ nixgl.overlay ]`.
6. Remove the `nixgl` flake input and `extraSpecialArgs`/`inherit nixgl` from
   both `home-manager/flake.nix` and `nixos/flake.nix`.

## Consequences

### Positive
- ✅ **Declarative and reproducible**: no manually placed AppImage, no SHA256
  to babysit; version pinning comes from nixpkgs.
- ✅ **Fewer moving parts**: one package replaces a wrapper, a helper, a
  desktop file, and an icon extraction.
- ✅ **Correct graphics**: NixOS provides Mesa/VAAPI directly; nixGL no longer
  interposes.
- ✅ **Upstream maintenance**: libxml2, fonts, icon, and desktop integration
  are handled by nixpkgs.
- ✅ **Fixed desktop integration**: the package's `.desktop` file registers the
  `x-scheme-handler/viber` MIME type.

### Negative
- ⚠️ **XWayland**: the package forces `QT_QPA_PLATFORM=xcb`, so Viber runs
  through XWayland rather than natively on Wayland.
- ⚠️ **Version lag**: the package tracks nixpkgs and pins to an Internet
  Archive snapshot, so it may trail the latest Viber release.

### Neutral
- Window management continues via the existing sway rule
  (`assign [instance="Viber"] workspace 3`); the Wayland/X11 app id should be
  re-verified at runtime after the switch.

## References

- [nixpkgs PR #396195: fix libxml2 breakage in viber][pr396195]
- [ADR-0011: Viber nixGL Integration][adr0011]
- [ADR-0008: Viber AppImage Packaging Solution][adr0008]

[pr396195]: https://github.com/NixOS/nixpkgs/pull/396195
[adr0011]: 0011-viber-nixgl-integration.md
[adr0008]: 0008-viber-appimage-packaging-solution.md
