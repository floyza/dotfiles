{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-master.url = "github:nixos/nixpkgs/master";
    # nixpkgs.url = "/home/gavin/src/nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/NUR";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    ssbm.url = "github:djanatyn/ssbm-nix";
    # ssbm.url = "/home/gavin/src/ssbm-nix";

    # My secrets are currently stored plaintext in my nix store, but I at least don't want to commit them to git
    # so I split them off here
    secrets.url = "/home/gavin/src/dotfiles/secrets";
    secrets.flake = false;
  };
  outputs = { self, home-manager, nur, nixpkgs, nixpkgs-master
    , emacs-overlay, ssbm, secrets, ... }@attrs: {
      nixosConfigurations = let
        system = "x86_64-linux";
        master = (import nixpkgs-master {
          inherit system;
          config.allowUnfree = true;
        });
      in {
        dreadnought = nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs =
            attrs; # pass each of out inputs to each module, eg. configuration.nix
          modules = [
            ./configuration.nix
            ./modules/japanese
            # ./modules/ssbm
            # ./modules/hotspot
            ssbm.nixosModule
            home-manager.nixosModules.home-manager
            {
              nixpkgs.overlays = [
                nur.overlay
                emacs-overlay.overlay
              ];
              nix.registry.nixpkgs.flake = nixpkgs;
              nix.nixPath =
                [ "nixpkgs=${nixpkgs}" ]; # use this instead of `nixos` channel
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                users.gavin = { imports = [ ./home.nix ]; };
                extraSpecialArgs = attrs;
              };
            }
          ];
        };
      };
    };
}
