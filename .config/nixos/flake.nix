{
  description = "NixOS system configuration for pavlos@localhost";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
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
    nixosConfigurations.localhost-nixos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./configuration.nix
        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.pavlos = import ../home-manager/home.nix;
          home-manager.extraSpecialArgs = { inherit nixgl dotfiles; };
        }
      ];
    };
  };
}
