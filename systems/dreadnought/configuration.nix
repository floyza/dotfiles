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
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "dreadnought";
  networking.interfaces.enp4s0.useDHCP = true;

  services.logind.extraConfig = ''
    HandlePowerKey=ignore
    HandleRebootKey=ignore
  '';

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
      SUBVOLUME = "/home/gavin/my";
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

  networking = {
    firewall.allowedTCPPorts = [ 5357 ]; # samba-wsdd
    firewall.allowedUDPPorts = [ 3702 ]; # samba-wsdd

    # possibly required for our samba discovery (https://wiki.archlinux.org/index.php/Samba#.22Browsing.22_network_fails_with_.22Failed_to_retrieve_share_list_from_server.22)
    firewall.extraCommands = ''
      iptables -t raw -A OUTPUT -p udp -m udp --dport 137 -j CT --helper netbios-ns
    '';
  };

  users.users.samba = {
    isSystemUser = true;
    group = "samba";
  };
  users.groups.samba = { };

  services.samba = {
    enable = true;
    openFirewall = true;
    securityType = "user";
    enableNmbd = false; # we use wsdd instead
    enableWinbindd = true; # need to look into more
    extraConfig = ''
      workgroup = WORKGROUP
      protocol = SMB3
      # server string = smbnix
      # netbios name = smbnix # we aren't using nmbd so I think we don't need this
      hosts allow = 192.168.0.0/24 10.42.0.0/24 127.0.0.1
      hosts deny = 0.0.0.0/0
      guest account = samba
      map to guest = bad user
    '';
    shares = {
      family = {
        comment = "Public Share";
        path = "/shares/Public";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "yes";
        "create mask" = "0644";
        "directory mask" = "0755";
      };
    };
  };

  services.samba-wsdd = { enable = true; };

  system.stateVersion = "21.11";
}