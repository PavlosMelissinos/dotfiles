{
  description = "Home Manager configuration of pavlos";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixgl = {
      url = "github:nix-community/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-software-center = {
      url = "github:PavlosMelissinos/nix-software-center";
      inputs.nixpkgs.follows = "nixpkgs";
    };
#    dotfiles = {
#      url = "github:PavlosMelissinos/dotfiles";
#      flake = false;  # Treat as source, not flake
#    };
  };

  # outputs = { nixpkgs, home-manager, nixgl, dotfiles, ... }:
  outputs = { nixpkgs, home-manager, nixgl, nix-software-center, nixpkgs-unstable, ... }:
    let
      pkgs = nixpkgs.legacyPackages."x86_64-linux";
      unstable-pkgs = import nixpkgs-unstable {
        system = "x86_64-linux";
        config.allowUnfreePredicate = pkg:
          builtins.elem (pkgs.lib.getName pkg) ["beeper" "claude-code"];
      };
    in {
      homeConfigurations = {
        pavlos = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;

          # Specify your home configuration modules here, for example,
          # the path to your home.nix.
          modules = [
            ({ ... }: {
              nixpkgs.overlays = [
                (final: prev: {
                  beeper = unstable-pkgs.beeper;
                  claude-code = unstable-pkgs.claude-code;
                  opencode = unstable-pkgs.opencode;
                })
              ];
            })
            ./home.nix
          ];

          # Pass nixGL and dotfiles to home.nix
          extraSpecialArgs = {
            inherit nixgl;
            # inherit dotfiles;
            inherit nix-software-center;
          };
        };
        pmelissinos = home-manager.lib.homeManagerConfiguration {
          inherit pkgs; 
    
          modules = [
            ({ ... }: {
              nixpkgs.overlays = [
                (final: prev: {
                  beeper = unstable-pkgs.beeper;
                  claude-code = unstable-pkgs.claude-code;
                  opencode = unstable-pkgs.opencode;
                })
              ];
            })

            ./home.nix # Use pavlos home.nix as a base

            ({ lib, ... }: {
            home.username = lib.mkForce "pmelissinos";
            home.homeDirectory = lib.mkForce "/home/pmelissinos";
          })
         ];

          # Pass nixGL and dotfiles to home.nix
          extraSpecialArgs = {
            inherit nixgl;
            # inherit dotfiles;
            inherit nix-software-center;
          };
        };
      };
    };
}
