{ config, pkgs, ... }:

{
  my.customData = {
    primaryAudio =
      "alsa_output.usb-C-Media_Electronics_Inc._USB_Audio_Device-00.analog-stereo";
    secondaryAudio =
      "alsa_output.usb-Burr-Brown_from_TI_USB_Audio_DAC-00.iec958-stereo";
    primaryOutput = {
      id = "DP-1";
      mode = "3440x1440@160.000hz";
      fps = 160;
    };
    musicDirectory = "/home/gavin/music";
  };

  networking.hostName = "corvette";

  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

  system.stateVersion = "23.05"; # Did you read the comment?
}

