{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/NUR";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    ssbm.url = "github:djanatyn/ssbm-nix";
    sway.url = "/home/gavin/src/sway";
    sway.flake = false;
  };
  outputs =
    { self, home-manager, nur, nixpkgs, emacs-overlay, ssbm, sway, ... }:
    let
      common = { pkgs, config, ... }: {
        nixpkgs.overlays = [
          nur.overlay
          emacs-overlay.overlay
          (self: super: {
            sway-unwrapped = super.sway-unwrapped.overrideAttrs (oldAttrs: {
              version = "custom-git";
              src = sway;
            });
            wlroots = super.wlroots.overrideAttrs (oldAttrs: {
              version = "git";
              src = self.fetchFromGitLab {
                domain = "gitlab.freedesktop.org";
                owner = "wlroots";
                repo = "wlroots";
                rev = "511f137f8fb245e4877d83a0846294091373eba1";
                sha256 = "sha256-yvB3V5OnS4IdkiR4co9xUEhhGy9Tslsg7Hr7B2PjZfs=";
              };
            });
          })
        ];
        nix.registry.nixpkgs.flake = nixpkgs;
      };
    in {
      nixosConfigurations = {
        dreadnought = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./configuration.nix
            ./modules/duckdns
            ssbm.nixosModule
            ./modules/ssbm
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
    };
}
