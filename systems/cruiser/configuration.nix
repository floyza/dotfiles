{
  config,
  lib,
  pkgs,
  ...
}:

{
  my.customData = {
    primaryAudio = "alsa_output.pci-0000_00_1b.0.analog-stereo";
    secondaryAudio = "alsa_output.pci-0000_00_1b.0.analog-stereo";
    primaryOutput = {
      id = "eDP-1";
      mode = "1600x900@59.985hz";
      fps = 60;
    };
    musicDirectory = "/home/gavin/music";
  };

  networking.hostName = "cruiser";

  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.editor = false;
  boot.loader.efi.canTouchEfiVariables = true;

  services.fstrim.enable = true;

  system.stateVersion = "24.11"; # Did you read the comment?
}
