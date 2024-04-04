{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/release-23.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/NUR";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    # ssbm.url = "github:jumper149/ssbm-nix/mbedtls2";
    # ssbm.url = "github:djanatyn/ssbm-nix";
    # ssbm.url = "github:lytedev/ssbm-nix";
    # ssbm.inputs.nixpkgs.follows = "nixpkgs";

    discocss.url = "github:floyza/discocss/discord-bugfix";
    discocss.flake = false;

    # My secrets are currently stored plaintext in my nix store, but I at least don't want to commit them to git
    # so I split them off here
    secrets.url = "/home/gavin/src/dotfiles/secrets";
    secrets.flake = false;
  };
  outputs = { self, home-manager, nur, nixpkgs, nixpkgs-unstable, emacs-overlay
    , discocss, secrets, ... }@attrs: {
      nixosConfigurations = let
        system = "x86_64-linux";
        unstable = (import nixpkgs-unstable {
          inherit system;
          config.allowUnfree = true;
        });
        update-overlays = [
          (self: super: {
            zef = self.symlinkJoin {
              name = "zef-wrapped";
              paths = [ super.zef ];
              nativeBuildInputs = [ self.makeWrapper ];
              postBuild = ''
                wrapProgram $out/bin/zef --set LD_LIBRARY_PATH ${self.readline70}/lib
              '';
            };
            rakudo = self.symlinkJoin {
              name = "rakudo-wrapped";
              paths = [ super.rakudo ];
              nativeBuildInputs = [ self.makeWrapper ];
              postBuild = ''
                wrapProgram $out/bin/rakudo --set LD_LIBRARY_PATH ${self.readline70}/lib
              '';
            };
            aseprite = self.callPackage ./packages/aseprite { };
            ncmpcpp = super.ncmpcpp.overrideAttrs (oldAttrs: {
              version = "master-thing";
              nativeBuildInputs = oldAttrs.nativeBuildInputs
                ++ [ self.autoreconfHook ];
              src = self.fetchFromGitHub {
                owner = "ncmpcpp";
                repo = "ncmpcpp";
                rev = "9f44edf0b1d74da7cefbd498341d59bc52f6043f";
                sha256 = "sha256-PjCzo3OSj/QIi2fdeV28ZjPiqLf6XAnZeNrDyjXt5wU=";
              };
            });
            discocss = super.discocss.overrideAttrs (oldAttrs: {
              version = "my-fork";
              src = discocss;
            });
          })
        ];
      in builtins.mapAttrs (sys: _:
        let sys-path = ./. + "/systems/${sys}";
        in nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs =
            attrs; # pass each of out inputs to each module, eg. configuration.nix
          modules = [
            (sys-path + "/configuration.nix")
            (sys-path + "/hardware-configuration.nix")

            ./common/settings.nix
            ./modules/japanese
            ./common/configuration.nix
            {
              home-manager.users.gavin = {
                imports = [ ./common/home.nix (sys-path + "/home.nix") ];
              };
            }

            home-manager.nixosModules.home-manager
            {
              nixpkgs.overlays = [ nur.overlay emacs-overlay.overlay ]
                ++ update-overlays;
              nix.registry.nixpkgs.flake = nixpkgs;
              nix.nixPath =
                [ "nixpkgs=${nixpkgs}" ]; # use this instead of `nixos` channel
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                extraSpecialArgs = attrs;
              };
            }
          ];
        }) (builtins.readDir ./systems);
    };
}
