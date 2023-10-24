{ config, lib, pkgs, ... }:

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
    musicDirectory = "/home/gavin/mnt/music";
  };

  # setup uefi
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.editor = false;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "dreadnought";
  networking.interfaces.enp4s0.useDHCP = true;

  # remove long delay on startup caused by waiting for availability of ip address
  networking.dhcpcd.extraConfig = ''
    noarp
  '';

  services.fstrim.enable = true;

  services.logind.extraConfig = ''
    HandlePowerKey=ignore
    HandleRebootKey=ignore
  '';

  users.users.gavin.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAID94ckwO1qnrewCT8QBou/8x+Wj7IUg9x+1/qn25IhVz gavin@Acer-Nitro-5" # acer laptop
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPkz94VB3wSbpCOIvaQ1f9ElZtmkfwQUK2dLdNJ33REa gavin.downard@runbox.com" # hp 655 laptop
  ];

  services.openssh.enable = true;
  services.openssh.ports = [ 22 ];
  services.openssh.openFirewall = true;
  services.openssh.settings = {
    PasswordAuthentication = false;
    GatewayPorts = "yes";
  };

  virtualisation.docker.storageDriver = "btrfs"; # NOTE: system specific?

  services.fail2ban.enable = true; # for ssh

  services.snapper.configs = {
    home = {
      SUBVOLUME = "/home/gavin";
      ALLOW_USERS = [ "gavin" ];
      TIMELINE_CREATE = true;
      TIMELINE_CLEANUP = true;
      TIMELINE_LIMIT_HOURLY = "5";
      TIMELINE_LIMIT_DAILY = "7";
      TIMELINE_LIMIT_WEEKLY = "0";
      TIMELINE_LIMIT_MONTHLY = "0";
      TIMELINE_LIMIT_YEARLY = "0";
    };
    personal = {
      SUBVOLUME = "/home/gavin/docs";
      ALLOW_USERS = [ "gavin" ];
      TIMELINE_CREATE = true;
      TIMELINE_CLEANUP = true;
      TIMELINE_LIMIT_HOURLY = "10";
      TIMELINE_LIMIT_DAILY = "10";
      TIMELINE_LIMIT_WEEKLY = "0";
      TIMELINE_LIMIT_MONTHLY = "10";
      TIMELINE_LIMIT_YEARLY = "1";
    };
    games = {
      SUBVOLUME = "/home/gavin/games";
      ALLOW_USERS = [ "gavin" ];
      TIMELINE_CREATE = true;
      TIMELINE_CLEANUP = true;
      TIMELINE_LIMIT_HOURLY = "5";
      TIMELINE_LIMIT_DAILY = "7";
      TIMELINE_LIMIT_WEEKLY = "0";
      TIMELINE_LIMIT_MONTHLY = "0";
      TIMELINE_LIMIT_YEARLY = "0";
    };
    games-other = {
      SUBVOLUME = "/home/gavin/games-other";
      ALLOW_USERS = [ "gavin" ];
      TIMELINE_CREATE = true;
      TIMELINE_CLEANUP = true;
      TIMELINE_LIMIT_HOURLY = "5";
      TIMELINE_LIMIT_DAILY = "7";
      TIMELINE_LIMIT_WEEKLY = "0";
      TIMELINE_LIMIT_MONTHLY = "0";
      TIMELINE_LIMIT_YEARLY = "0";
    };
  };

  system.stateVersion = "21.11";
}
