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
    opencode-flake.url = "github:aodhanhayter/opencode-flake";
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
  outputs = { nixpkgs, home-manager, nixgl, opencode-flake, nix-software-center, nixpkgs-unstable, ... }:
    let
      pkgs = nixpkgs.legacyPackages."x86_64-linux";
      unstable-pkgs = import nixpkgs-unstable {
        system = "x86_64-linux";
        config.allowUnfreePredicate = pkg:
          builtins.elem (pkgs.lib.getName pkg) ["beeper"];
      };
    in {
      homeConfigurations."pavlos" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        # Specify your home configuration modules here, for example,
        # the path to your home.nix.
        modules = [
          ({ ... }: {
            nixpkgs.overlays = [
              (final: prev: {
                beeper = unstable-pkgs.beeper;
              })
            ];
          })
      ./home.nix
    ];

        # Pass nixGL and dotfiles to home.nix
#        extraSpecialArgs = { inherit nixgl dotfiles; };
        extraSpecialArgs = {
          inherit nixgl;
          inherit opencode-flake;
          inherit nix-software-center;
        };
      };
    };
}
