{
  config,
  lib,
  pkgs,
  ...
}:

{

  home.packages = with pkgs; [
    (btop.override { rocmSupport = true; })
    ### programming
    ## resources
    godot_4
    ### games
    retroarchFull
    obs-studio
    pcsx2
    runelite
    pyfa
  ];

  home.stateVersion = "21.11";
}
