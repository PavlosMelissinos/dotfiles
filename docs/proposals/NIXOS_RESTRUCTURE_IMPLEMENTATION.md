# NixOS + Home-Manager Restructure Implementation Guide

## Overview
This document provides detailed implementation steps for restructuring the NixOS configuration to use a unified `.config/nix/` directory structure with complete home directory git integration.

## Current State Analysis
- **Current structure**: Split between `.config/nixos/` and `.config/home-manager/`
- **Issue**: Broken import path `../home-manager/home.nix` in flake.nix
- **Goal**: Single unified configuration with home directory as git repo

## File Operations Required

### 1. Create Directory Structure
```bash
mkdir -p ~/.config/nix
```

### 2. File Moves
Execute these moves in order:

```bash
# Move core configuration files
mv ~/.config/nixos/flake.nix ~/.config/nix/flake.nix
mv ~/.config/nixos/configuration.nix ~/.config/nix/configuration.nix
mv ~/.config/home-manager/home.nix ~/.config/nix/home.nix

# Move supporting files
mv ~/.config/nixos/nixos-install.sh ~/.config/nix/nixos-install.sh
mv ~/.config/nixos/README-HARDWARE-CONFIG.md ~/.config/nix/README-HARDWARE-CONFIG.md
mv ~/.config/nixos/hardware-configuration.nix.template ~/.config/nix/hardware-configuration.nix.template
mv ~/.config/nixos/.gitignore ~/.config/nix/.gitignore

# Preserve VM files if they exist
if [ -f ~/.config/nixos/OVMF_VARS.fd ]; then
    mv ~/.config/nixos/OVMF_VARS.fd ~/.config/nix/OVMF_VARS.fd
fi
```

### 3. Update flake.nix
Replace the entire flake.nix content with:

```nix
{
  description = "NixOS system configuration for pavlos@localhost";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixgl = {
      url = "github:nix-community/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dotfiles = {
      url = "github:PavlosMelissinos/dotfiles";
      flake = false;  # Treat as source, not flake
    };
  };

  outputs = { self, nixpkgs, home-manager, nixgl, dotfiles }: {
    # NixOS system configuration
    nixosConfigurations.localhost-nixos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./configuration.nix
        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.pavlos = import ./home.nix;
          home-manager.extraSpecialArgs = { inherit nixgl dotfiles; };
        }
      ];
    };

    # Home Manager standalone configuration
    homeConfigurations.pavlos = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      modules = [
        ./home.nix
        {
          home = {
            username = "pavlos";
            homeDirectory = "/home/pavlos";
            stateVersion = "23.11";
          };
        }
      ];
      extraSpecialArgs = { inherit nixgl dotfiles; };
    };
  };
}
```

### 4. Update home.nix
Add git clone integration to home.nix by inserting this activation script after the existing content:

```nix
  # Git repository setup for home directory
  home.activation.setupDotfilesRepo = lib.hm.dag.entryAfter ["writeBoundary"] ''
    # Clone dotfiles repository to home directory if not already present
    if [ ! -d "$HOME/.git" ]; then
      $DRY_RUN_CMD ${pkgs.git}/bin/git clone https://github.com/PavlosMelissinos/dotfiles.git /tmp/dotfiles-temp
      $DRY_RUN_CMD ${pkgs.git}/bin/git --git-dir=/tmp/dotfiles-temp/.git --work-tree="$HOME" checkout -f
      $DRY_RUN_CMD mv /tmp/dotfiles-temp/.git "$HOME/"
      $DRY_RUN_CMD rm -rf /tmp/dotfiles-temp
      $DRY_RUN_CMD ${pkgs.git}/bin/git --git-dir="$HOME/.git" --work-tree="$HOME" config core.worktree "$HOME"
    fi
  '';
```

### 5. Update .gitignore
Add these lines to ~/.gitignore:

```
!.config/nix/
!.config/nix/**
```

### 6. Update nixos-install.sh
Update the flake path in nixos-install.sh:

```bash
# Change this line:
# nixos-install --root /mnt --flake /mnt/etc/nixos#localhost-nixos --impure

# To this:
nixos-install --root /mnt --flake /mnt/etc/nixos#localhost-nixos --impure
```

### 7. Clean Up Old Directories
```bash
# Remove old directories after confirming files are moved
rmdir ~/.config/nixos
rmdir ~/.config/home-manager
```

## Expected Commands After Implementation

### System Rebuilds
```bash
sudo nixos-rebuild switch --flake ~/.config/nix#localhost-nixos --impure
```

### Home Manager Only
```bash
home-manager switch --flake ~/.config/nix#pavlos
```

### Fresh Installation
```bash
nixos-install --root /mnt --flake github:PavlosMelissinos/dotfiles?dir=.config/nix#localhost-nixos --impure
```

## Validation Steps

1. **Flake syntax check**:
   ```bash
   cd ~/.config/nix && nix flake check
   ```

2. **Home configuration test**:
   ```bash
   home-manager switch --flake ~/.config/nix#pavlos --dry-run
   ```

3. **System configuration test**:
   ```bash
   sudo nixos-rebuild dry-run --flake ~/.config/nix#localhost-nixos
   ```

4. **Git status check**:
   ```bash
   git status
   git add .config/nix/
   ```

## Post-Implementation Analysis

After successfully completing the restructuring, generate comprehensive analysis reports to guide future migration to more declarative management.

### 1. Generate Dotfiles Migration Analysis
Create a detailed report analyzing current configuration files:

```bash
# Create comprehensive dotfiles analysis
cat > ~/docs/proposals/dotfiles-declarative-migration-analysis.md << 'EOF'
# Dotfiles Declarative Migration Analysis

## Overview
Analysis of current configuration files for potential migration to declarative home-manager management.

## Configuration Files Inventory

### Applications with Home-Manager Modules (High Priority)
- Firefox: ~/.config/mozilla/ → programs.firefox
- Git: ~/.gitconfig → programs.git (already done)
- Zsh: ~/.config/zsh/ → programs.zsh (already done)
- SSH: ~/.ssh/ → programs.ssh
- GPG: ~/.gnupg/ → programs.gpg

### Static Config Files (Medium Priority - home.file candidates)
- Sway: ~/.config/sway/ → home.file.".config/sway/config".source
- Alacritty: ~/.config/alacritty/ → home.file
- Emacs: ~/.config/emacs/ → home.file
- Waybar: ~/.config/waybar/ → home.file

### Dynamic/Development Configs (Keep Imperative)
- Application caches: ~/.cache/
- Runtime data: ~/.local/share/
- Temporary configs that change frequently

## Migration Recommendations

1. **Phase 1**: Convert apps with dedicated home-manager modules
2. **Phase 2**: Move static configs to home.file management
3. **Phase 3**: Evaluate remaining files case-by-case

## Implementation Examples
[Specific examples would be added during actual analysis]
EOF
```

### 2. Generate Executable Scripts Migration Analysis
Create detailed analysis of all executable scripts:

```bash
# Analyze executable scripts throughout home directory
find ~ -type f -executable -not -path "*/.*" -not -path "*/workspace/*" -not -path "*/Downloads/*" > /tmp/executables_list.txt

# Create comprehensive scripts analysis
cat > ~/docs/proposals/executable-scripts-migration-analysis.md << 'EOF'
# Executable Scripts Migration Analysis

## Overview
Analysis of executable scripts for potential migration to nix-managed solutions.

## Scripts Inventory (~/.local/bin/ and other locations)

### Convert to Nix Packages (High Priority)
Scripts that could become proper nix derivations:
- alda, bb, jet: Development tools → nix packages or overlay
- Browser wrappers: chromium, firefox-wrapper → writeShellScriptBin
- System utilities: pavolume → writeShellScriptBin (already in git)

### Convert to home.file (Medium Priority)
Static scripts that don't change:
- maintenance-runner.sh, system-cleanup.sh → home.file.".local/bin/*"
- u2f-*.sh scripts → home.file management
- update-gammastep-location → writeShellScriptBin

### Keep Imperative (Development/Changing)
Scripts under active development or frequent modification:
- debug-* utilities
- download_youtube_playlists.sh (user script)
- Custom development tools

## Migration Strategy

1. **Phase 1**: Convert system utilities to writeShellScriptBin
2. **Phase 2**: Package development tools properly
3. **Phase 3**: Move static scripts to home.file
4. **Phase 4**: Evaluate remaining for nix-store benefits

## Implementation Examples
[Specific conversion examples would be added during actual analysis]
EOF
```

## Critical Success Criteria

### Core Restructuring
- [ ] All files successfully moved to ~/.config/nix/
- [ ] flake.nix syntax is valid and evaluates correctly
- [ ] Both nixos-rebuild and home-manager commands work
- [ ] Git tracking includes new .config/nix/ directory
- [ ] Old directories are completely removed
- [ ] Home activation script for git setup is functional

### File Organization & Analysis
- [ ] Dotfiles migration analysis report generated
- [ ] Executable scripts migration analysis report generated
- [ ] Both reports contain actionable recommendations with migration phases

## Rollback Plan

If issues occur:
1. Keep backup of original directories before deletion
2. Restore from git history if needed
3. Original flake can be restored from git log

## Post-Implementation Benefits

### Immediate Benefits
1. **Single configuration location**: All Nix configs in ~/.config/nix/
2. **Dual command support**: Both system and home-only rebuilds
3. **Git-native home management**: Home directory IS the dotfiles repo
4. **Single-command installation**: Complete system setup from GitHub
5. **Unified workflow**: Consistent path structure across all operations

### Strategic Benefits
6. **Migration roadmap**: Clear path for progressive declarative management
7. **Reduced manual configuration**: Systematic approach to automate config management
8. **Better separation of concerns**: Nix-managed vs user-managed files clearly identified
9. **Future-proofing**: Foundation laid for ongoing migration to full declarative setup

## Notes for Executing Agent

- Preserve all existing package lists and service configurations
- Maintain hardware-configuration.nix fallback logic in configuration.nix
- Ensure all existing functionality (nixGL, services, etc.) remains intact
- Test each step thoroughly before proceeding to cleanup
- Validate flake syntax before committing changes
