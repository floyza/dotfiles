{ config, lib, pkgs, ssbm, ... }:

{

  home.packages = with pkgs; [
    ### programming
    ## resources
    godot_4
    ### games
    # slippi-netplay
    ssbm.packages.x86_64-linux.slippi-netplay # why doesn't the overlay work?
    yuzu-mainline
    retroarchFull
  ];
}
