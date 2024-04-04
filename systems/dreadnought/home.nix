{ config, lib, pkgs, ... }:

{

  home.packages = with pkgs; [
    ### programming
    ## resources
    godot_4
    ### games
    yuzu-mainline
    retroarchFull
    obs-studio
  ];

  home.stateVersion = "21.11";
}
