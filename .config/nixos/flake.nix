{
  description = "NixOS system configuration for pavlos@localhost";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-software-center = {
      url = "github:PavlosMelissinos/nix-software-center";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dotfiles = {
      url = "github:PavlosMelissinos/dotfiles";
      flake = false;  # Treat as source, not flake
    };
  };

  outputs = { self, nixpkgs, home-manager, nix-software-center, dotfiles }:
    let
      lib = nixpkgs.lib;

      # Both pavlos and pmelissinos exist as plain system accounts on
      # every machine (see users.users in configuration.nix), but each
      # host only gets a home-manager profile for the account that
      # actually logs into it. Keyed by networking.hostName
      # (configuration.nix defaults it to "legion", the work machine;
      # the home machine overrides it with a plain assignment in its
      # own host-specific hardware-configuration.nix).
      hostPrimaryUser = {
        legion = "pmelissinos";
        # home-machine-hostname = "pavlos"; # add once that host exists
      };

      # home.nix is shaped for pavlos (home.username/homeDirectory are
      # hardcoded at its top); any other account needs the same
      # mkForce override the standalone home-manager flake
      # (.config/home-manager/flake.nix) already uses for pmelissinos.
      homeModulesFor = user:
        [ ../home-manager/home.nix ]
        ++ lib.optional (user != "pavlos") {
          home.username = lib.mkForce user;
          home.homeDirectory = lib.mkForce "/home/${user}";
        };
    in {
      nixosConfigurations.localhost-nixos = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./configuration.nix
          home-manager.nixosModules.home-manager
          ({ config, lib, ... }:
            let
              primaryUser = hostPrimaryUser.${config.networking.hostName}
                or (throw "Unknown host '${config.networking.hostName}': add it to hostPrimaryUser in flake.nix");
            in {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.${primaryUser}.imports =
                homeModulesFor primaryUser;
              home-manager.extraSpecialArgs = {
                inherit dotfiles nix-software-center;
              };
            })
        ];
      };
    };
}
