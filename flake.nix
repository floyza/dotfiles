{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/NUR";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };
  outputs = { self, home-manager, nur, nixpkgs, emacs-overlay, ... }:
    let
      system = "x86_64-linux";

      common = { pkgs, config, ... }: {
        nixpkgs.overlays = [ nur.overlay emacs-overlay.overlay ];
        nix.registry.nixpkgs.flake = nixpkgs;
      };

    in {
      nixosConfigurations.dreadnought = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          ./configuration.nix
          ./modules/duckdns
          common
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              users.gavin = { imports = [ ./home.nix ]; };
            };
          }
        ];
      };
    };
}
