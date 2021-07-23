{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay/master";
    nix-doom-emacs.url = "github:vlaci/nix-doom-emacs";
    nix-doom-emacs.inputs.emacs-overlay.follows = "emacs-overlay";
    nur.url = "github:nix-community/NUR";
  };
  outputs = { self, home-manager, emacs-overlay, nur, nix-doom-emacs, nixpkgs, ... }:
    let
      system = "x86_64-linux";

      common = { pkgs, config, ... }: {
        nixpkgs.overlays = [ emacs-overlay.overlay nur.overlay ];
        nix.registry.nixpkgs.flake = nixpkgs;
      };

    in {
      nixosConfigurations.gavin-nixos = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          ./configuration.nix
          common
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              users.gavin = {
                imports = [
                  nix-doom-emacs.hmModule
                  ./home.nix
                ];
              };
            };
          }
        ];
      };
    };
}
