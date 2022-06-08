{ config, pkgs, lib, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  boot.kernelPackages = pkgs.linuxPackages;
  boot.kernel.sysctl = { "kernel.sysrq" = "1"; };

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  time.timeZone = "America/Los_Angeles";

  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [
    (self: super: {
      steam = super.steam.override { extraPkgs = pkgs: [ pkgs.libpng ]; };
    })
  ];

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking = {
    hostName = "dreadnought";
    networkmanager.enable = true;
    enableIPv6 = false; # vpn might leak if true

    useDHCP = false;
    interfaces.enp4s0.useDHCP = true;
    # automatically opened tcp ports: ssh
    # manually opened: murmur, samba, samba-wsdd
    # we don't use samba.openFirewall since we only need 445 open: we don't use nmbd so the other ports are unnessesary
    firewall.allowedTCPPorts = [ config.services.murmur.port 445 5357 41230 ];
    # automatically opened udp ports: avahi
    # manually opened: samba-wsdd
    firewall.allowedUDPPorts = [ 3702 ];
    firewall.enable = true;
    firewall.allowPing = true;
    # possibly required for our samba discovery (https://wiki.archlinux.org/index.php/Samba#.22Browsing.22_network_fails_with_.22Failed_to_retrieve_share_list_from_server.22)
    firewall.extraCommands =
      "iptables -t raw -A OUTPUT -p udp -m udp --dport 137 -j CT --helper netbios-ns";

    hosts = {
      "192.168.0.2" = [ "dadbox" ];
      "192.168.0.3" = [ "remotehost" ];
      "192.168.0.4" = [ "donbox" ];
    };
  };

  services.chrony.enable = true;

  services.logind.extraConfig = ''
    HandlePowerKey=ignore
  '';

  services.avahi = {
    enable = true;
    interfaces = [ "enp4s0" ];
    publish = {
      enable = true;
      domain = false;
      userServices = true;
    };
  };

  services.invidious.enable = false;

  services.snapper.configs = {
    home = {
      subvolume = "/home/gavin";
      extraConfig = ''
        ALLOW_USERS="gavin"
        TIMELINE_CREATE=yes
        TIMELINE_CLEANUP=yes
        TIMELINE_LIMIT_HOURLY="5"
        TIMELINE_LIMIT_DAILY="7"
        TIMELINE_LIMIT_WEEKLY="0"
        TIMELINE_LIMIT_MONTHLY="5"
        TIMELINE_LIMIT_YEARLY="0"
      '';
    };
    personal = {
      subvolume = "/home/gavin/my";
      extraConfig = ''
        ALLOW_USERS="gavin"
        TIMELINE_CREATE=yes
        TIMELINE_CLEANUP=yes
        TIMELINE_LIMIT_HOURLY="10"
        TIMELINE_LIMIT_DAILY="10"
        TIMELINE_LIMIT_WEEKLY="0"
        TIMELINE_LIMIT_MONTHLY="10"
        TIMELINE_LIMIT_YEARLY="10"
      '';
    };
    games = {
      subvolume = "/home/gavin/games";
      extraConfig = ''
        ALLOW_USERS="gavin"
        TIMELINE_CREATE=yes
        TIMELINE_CLEANUP=yes
        TIMELINE_LIMIT_HOURLY="5"
        TIMELINE_LIMIT_DAILY="7"
        TIMELINE_LIMIT_WEEKLY="0"
        TIMELINE_LIMIT_MONTHLY="0"
        TIMELINE_LIMIT_YEARLY="0"
      '';
    };
  };

  services.murmur = {
    enable = true;
    bonjour = true;
    welcometext = "Welcome to my humble server.";
  };

  programs.sway = { enable = true; };

  # Enable pipewire
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  security.pam.loginLimits = [{
    domain = "gavin";
    item = "nofile";
    type = "hard";
    value = 1048576;
  }];

  users.users.gavin = {
    isNormalUser = true;
    extraGroups = [
      "audio"
      "wheel"
      "libvirtd"
      "networkmanager"
      "scanner"
      "lp"
      "adbusers"
      "docker"
    ];
    shell = pkgs.zsh;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPDx/aVrNg3oc/+UEAOi2D2dbBXQCwQCaVtUBspyuD5O gavin.downard@runbox.com"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC/rby0TesyqNRmgZPxbFWBkJH81kaaF0mFavRfNPlAlBtYm1a2VuT7758fg8J2b/qr1OA976nPEBwn9j3uL5NmkH0RWlr+R88Ob5KO9pux6G9WRvgUixWFYupL/BioZpXGsQ5+6jDK10xz1iY5fUwrA0LtGqguv5gYLDDI5KPaSudNm6P3Rd0vfJnw700xywgR/nD7Fw6gFGbb0v/utq5TFH6wRin62BWtUaJ4vlEtLuk5I7BxFB2aREbfjci57Nj1/zHoE+4QZshhn41zuRyH3Va+yjjhBFWm48q4bykOoLUb/CQiAhnmtdWjamWPohpQWtX1c2fWAeYYepRRzpZ5pl2e430aKsivsAB/wwbvjRLOTtSpvWc41hFRUitqtPk7R4uSXd+/BPVu8JuPGt4SziNVeTcvTEqz52vV1fSv1dAb4z55bA+h0WI1aKtkvmMemfeRD/4Ljk/yADkjGflYo94NsnpVWmadBJtIKFVPpsy9sN/mVk+xP096ShqJRdk= mobian@mobian"
    ];
  };

  services.locate = {
    enable = true;
    locate = pkgs.mlocate;
    localuser = null; # for pkgs.mlocate
    interval = "hourly"; # fine with mlocate?
    pruneNames = [ ".snapshots" ];
  };

  services.openvpn.servers = {
    tcp = {
      config = "config /root/nix/openvpn/us8272.nordvpn.com.tcp.conf ";
      autoStart = false; # my auth doesn't work right now
    };
  };

  environment.systemPackages = (with pkgs; [
    wget
    vim
    tmux
    efibootmgr
    unzip
    inetutils
    usbutils # lsusb

    virt-manager
    dconf # needed for saving settings in virt-manager
    libguestfs # needed for virt-sparsify
  ]);

  services.udev.packages = [ pkgs.qmk-udev-rules ];

  services.udev.extraRules = ''
    ACTION=="add", ATTR{idVendor}=="26ce", ATTR{idProduct}=="01a2", RUN="${pkgs.bash}/bin/bash -c 'echo 0 >/sys/\$devpath/authorized'"
    ACTION=="add", ATTR{idVendor}=="1b1c", ATTR{idProduct}=="0c1a", RUN="${pkgs.bash}/bin/bash -c 'echo 0 >/sys/\$devpath/authorized'"
  '';

  # require modification to udev rules
  programs.adb.enable = true;

  environment.pathsToLink = [
    "/share/zsh" # for zsh completion
    "/libexec"
  ]; # for polkit-gnome

  programs.zsh.enable = true;
  programs.zsh.enableCompletion = true;
  programs.bash.enableCompletion = true;
  users.defaultUserShell = pkgs.zsh;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryFlavor = "gtk2";
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.passwordAuthentication = false;
  services.openssh.ports = [ 22 ];
  services.openssh.gatewayPorts = "yes";

  users.users.samba = {
    isSystemUser = true;
    group = "samba";
  };
  users.groups.samba = { };

  services.samba = {
    enable = true;
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

  services.samba-wsdd = {
    enable = true;
    interface = "enp4s0";
  };

  security.polkit.enable = true;

  virtualisation.libvirtd.enable = true;
  virtualisation.docker.enable = true;
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
  programs.dconf.enable = true;

  hardware.opengl = {
    driSupport = true;
    driSupport32Bit = true;
  };

  hardware.steam-hardware.enable = true;

  nix = {
    settings = {
      trusted-public-keys = [
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
      ];
      substituters = [
        "https://hydra.iohk.io"
        "https://nix-community.cachix.org"
        "https://nixcache.reflex-frp.org"
      ];
      auto-optimise-store = true;
    };
    package = pkgs.nixFlakes;
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
      experimental-features = nix-command flakes
    '';
    registry = {
      my = {
        from.type = "indirect";
        from.id = "my";
        to = {
          type = "github";
          owner = "floyza";
          repo = "templates";
        };
      };
    };
  };

  documentation.info.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?

}
