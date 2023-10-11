{ config, lib, pkgs, ssbm, ... }:

{

  home.packages = with pkgs; [
    ### programming
    ## resources
    godot_4
    ### games
    ssbm.packages.x86_64-linux.slippi-launcher # TODO why doesn't the overlay work?
    yuzu-mainline
    retroarchFull
  ];

  xdg.configFile."Slippi Launcher/Settings".source = lib.mkForce
    (let jsonFormat = pkgs.formats.json { };
    in jsonFormat.generate "slippi-config" {
      settings = {
        isoPath =
          "/home/gavin/games/roms/melee/Super Smash Bros. Melee (USA) (En,Ja) (Rev 2).iso";
        autoUpdateLauncher = false;
      };
    });

  home.stateVersion = "21.11";
}
