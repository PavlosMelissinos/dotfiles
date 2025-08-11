# ADR-0011: Viber nixGL Integration and Dynamic Icon Extraction

**Date**: 2025-08-11
**Status**: Accepted
**Supersedes**: ADR-0008 (Viber AppImage Packaging Solution)

## Context

ADR-0008 established Viber AppImage integration but had several issues that
emerged through usage:

### Problems with Previous Solution
- **Font Rendering**: Monospaced fonts due to `appimage-run` isolation
- **Link Handling**: Links couldn't open Firefox due to library conflicts
- **Software Rendering**: Environment variables forced software rendering,
  degrading performance
- **Icon Management**: Generic icon instead of Viber's brand icon
- **Git Repository Size**: Storing 20KB PNG icon in version control

### Requirements for Improvement
- **Proper Font Rendering**: Use system fonts consistently
- **Working Link Integration**: Links must open in Firefox reliably
- **Hardware Acceleration**: Utilize GPU acceleration for better performance
- **Brand Consistency**: Use official Viber icon in desktop environments
- **Clean Repository**: Avoid storing binary assets in git

## Decision

We will implement nixGL-wrapped Viber with direct AppImage execution and dynamic
icon extraction.

### Core Architecture
1. **nixGL Wrapper**: Provide OpenGL context for hardware acceleration
2. **Direct Execution**: Bypass `appimage-run` to fix font rendering
3. **Custom xdg-open**: Clean Firefox environment for link handling
4. **Dynamic Icon**: Extract icon from AppImage during build
5. **Declarative Management**: All configuration through home.nix

### Implementation Strategy

#### 1. nixGL Integration
```nix
# Viber with nixGL wrapper and optimized configuration
(let
  xdg-open-firefox = pkgs.writeShellScriptBin "xdg-open" ''
    # Clean environment for Firefox to avoid library conflicts
    exec env -i \
      HOME="$HOME" \
      USER="$USER" \
      DISPLAY="$DISPLAY" \
      WAYLAND_DISPLAY="$WAYLAND_DISPLAY" \
      XDG_RUNTIME_DIR="$XDG_RUNTIME_DIR" \
      PATH="/home/pavlos/.nix-profile/bin:/usr/bin:/bin" \
      /home/pavlos/.nix-profile/bin/firefox "$@"
  '';
in
config.lib.nixGL.wrap (pkgs.writeShellScriptBin "viber" ''
  export PATH="${xdg-open-firefox}/bin:$PATH"
  # Direct AppImage execution for proper fonts
  if /home/pavlos/.local/bin/viber.AppImage "$@" < /dev/null; then
    exit 0
  else
    # Fallback to appimage-run if direct execution fails
    exec appimage-run /home/pavlos/.local/bin/viber.AppImage "$@" < /dev/null
  fi
''))
```

#### 2. Dynamic Icon Extraction
```nix
".local/share/icons/viber.png".source =
  let
    viberAppImage = builtins.fetchurl {
      url = "file:///home/pavlos/.local/bin/viber.AppImage";
      sha256 = "sha256-jwsePK1l/WI+stDNpAD1t2Obr1BwpNDP0nzkIDfGGoA=";
    };

    extracted = pkgs.appimageTools.extract {
      src = viberAppImage;
      pname = "viber";
      version = "1.0";
    };
  in
    "${extracted}/usr/share/icons/hicolor/256x256/apps/viber.png";
```

#### 3. Desktop Integration
```nix
".local/share/applications/viber.desktop".text = ''
  [Desktop Entry]
  Name=Viber
  Comment=Free calls, text and picture sharing with anyone, anywhere!
  Exec=${config.home.profileDirectory}/bin/viber %U
  Icon=${config.home.homeDirectory}/.local/share/icons/viber.png
  Terminal=false
  Type=Application
  Categories=Network;InstantMessaging;
  MimeType=x-scheme-handler/viber;
  StartupWMClass=ViberPC
  StartupNotify=true
'';
```

## Consequences

### Positive
- ✅ **Perfect Font Rendering**: Direct execution uses system fonts properly
- ✅ **Working Links**: Custom xdg-open wrapper opens Firefox reliably
- ✅ **Hardware Acceleration**: nixGL provides OpenGL context for GPU
  acceleration
- ✅ **Brand Icon**: Official Viber icon extracted dynamically from AppImage
- ✅ **Clean Repository**: No binary files stored in git
- ✅ **Reproducible Builds**: Content-addressed AppImage with SHA256
  verification
- ✅ **No User Prompts**: stdin redirection prevents interactive MIME prompts
- ✅ **Robust Fallback**: Falls back to appimage-run if direct execution fails

### Negative
- ⚠️ **Build Complexity**: More sophisticated build process with extraction
- ⚠️ **AppImage Dependency**: Requires AppImage file to exist at specified path
- ⚠️ **SHA256 Management**: Must update hash when Viber AppImage changes

### Neutral
- **Desktop Integration**: Standard .desktop file and icon management
- **Package Management**: Continues to be managed through home-manager

## Implementation Details

### Technical Improvements Over ADR-0008

| Aspect              | ADR-0008 (Previous)       | ADR-0011 (Current)              |
|---------------------|---------------------------|---------------------------------|
| **Font Rendering**  | Monospaced (appimage-run) | Proportional (direct execution) |
| **Link Handling**   | Broken                    | Working (custom xdg-open)       |
| **Graphics**        | Software rendering        | Hardware accelerated (nixGL)    |
| **Icon**            | Generic                   | Official Viber icon             |
| **Repository**      | 20KB PNG in git           | Dynamic extraction              |
| **User Experience** | Interactive prompts       | Silent operation                |

### Key Technical Solutions
1. **Font Fix**: Direct AppImage execution bypasses font isolation
2. **Link Fix**: Clean environment wrapper for Firefox
3. **Performance**: nixGL provides proper OpenGL context
4. **Icon**: Dynamic extraction with appimageTools.extract
5. **UX**: Stdin redirection prevents MIME type prompts

## Verification Results

- ✅ Viber launches with proper proportional fonts
- ✅ Links open reliably in Firefox
- ✅ Hardware acceleration functional (smooth UI)
- ✅ Official Viber icon appears in application launchers
- ✅ No binary files added to git repository
- ✅ Silent operation without user prompts
- ✅ Robust fallback mechanism working

This solution represents a significant improvement in user experience while
maintaining the architectural principles established in ADR-0008.
