{
  config,
  lib,
  pkgs,
  ...
}:

{
  my.customData = {
    primaryAudio = "alsa_output.usb-Cooler_Master_Technology_Inc._MH752_00000000-00.analog-stereo";
    secondaryAudio = "alsa_output.usb-Burr-Brown_from_TI_USB_Audio_DAC-00.analog-stereo";
    primaryOutput = {
      id = "DP-3";
      mode = "3440x1440@160.000hz";
      fps = 160;
    };
    musicDirectory = "/home/gavin/mnt/music";
  };

  services.ollama = {
    enable = true;
    acceleration = "rocm";
    port = 11434;
    rocmOverrideGfx = "10.3.0";
  };

  # setup uefi
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.editor = false;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "dreadnought";
  networking.interfaces.enp4s0.useDHCP = true;
  networking.nftables.enable = lib.mkForce false;

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

  virtualisation.waydroid.enable = true;

  services.samba = {
    enable = true;
    openFirewall = true;
    nmbd.enable = false; # we use wsdd instead
    winbindd.enable = true; # need to look into more
    settings = {
      global = {
        workgroup = "WORKGROUP";
        protocol = "SMB3";
        # server string = smbnix
        # netbios name = smbnix # we aren't using nmbd so I think we don't need this
        "hosts allow" = "192.168.0.0/24 10.42.0.0/24 127.0.0.1";
        "hosts deny" = "0.0.0.0/0";
        "guest account" = "samba";
        "map to guest" = "never";
        "valid users" = "samba";

        "force user" = "samba";

        security = "user";
      };

      # shares
      family = {
        comment = "Public Share";
        path = "/shares/Public";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "no";
        "create mask" = "0644";
        "directory mask" = "0755";
      };
    };
  };

  services.samba-wsdd = {
    enable = true;
  };

  system.stateVersion = "21.11";
}
