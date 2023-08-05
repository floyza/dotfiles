{ config, pkgs, ... }:

{
  my.customData = {
    primaryAudio = "alsa_output.pci-000_00_14.2.analog-stereo";
    primaryOutput = {
      id = "DP-1";
      mode = "1366x768@60.031";
      fps = 60;
    };
    musicDirectory = "/home/gavin/music";
  };

  networking.hostName = "corvette";

  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda";

  system.stateVersion = "23.05"; # Did you read the comment?
}

