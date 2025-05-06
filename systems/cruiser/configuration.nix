{
  config,
  lib,
  pkgs,
  ...
}:

{
  my.customData = {
    primaryAudio = "";
    secondaryAudio = "";
    primaryOutput = {
      id = "";
      mode = "";
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
