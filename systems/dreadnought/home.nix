{ config, lib, pkgs, ssbm, ... }:

{

  home.packages = with pkgs; [
    ### programming
    ## resources
    godot_4
    ### games
    yuzu-mainline
    retroarchFull
  ];

  home.stateVersion = "21.11";
}
