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
    godot
    ### games
    retroarchFull
    obs-studio
    pcsx2
    runelite
    pyfa
  ];

  home.stateVersion = "21.11";
}
