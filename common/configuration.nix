{ config, pkgs, lib, ... }:

{
  boot.kernelPackages = pkgs.linuxPackages;
  boot.kernel.sysctl = { "kernel.sysrq" = "1"; };

  time.timeZone = "America/Los_Angeles";

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.permittedInsecurePackages =
    [ "openssl-1.1.1w" ]; # yuck! devil daggers uses this version of the library
  nixpkgs.overlays = [
    (self: super: {
      steam = super.steam.override {
        extraPkgs = pkgs: [ pkgs.libpng pkgs.libsecret pkgs.openssl_1_1 ];
      };
    })
  ];

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  environment.etc."resolv.conf".text = ''
    nameserver 127.0.0.1
  '';
  networking = {
    networkmanager.enable = true;
    enableIPv6 = false; # vpn might leak if true

    useDHCP = false;
    # automatically opened tcp ports: murmur
    # 41230 is a custom port used for whatever stuff i temporarily need: games, etc
    firewall.allowedTCPPorts = [ 41230 ];
    # automatically opened udp ports: avahi
    # manually opened: factorio, custom
    firewall.allowedUDPPorts = [ 34197 41230 ];
    firewall.enable = true;
    firewall.allowPing = true;

    nftables.enable = true;

    hosts = {
      "192.168.0.2" = [ "dadbox" ];
      "192.168.0.3" = [ "remotehost" ];
      "192.168.0.4" = [ "donbox" ];
    };
  };

  # see https://nixos.wiki/wiki/Encrypted_DNS
  # and https://github.com/DNSCrypt/dnscrypt-proxy/blob/master/dnscrypt-proxy/example-dnscrypt-proxy.toml
  services.dnscrypt-proxy2 = {
    enable = true;
    settings = {
      ipv6_servers = false;
      require_dnssec = true;

      sources.public-resolvers = {
        urls = [
          "https://raw.githubusercontent.com/DNSCrypt/dnscrypt-resolvers/master/v3/public-resolvers.md"
          "https://download.dnscrypt.info/resolvers-list/v3/public-resolvers.md"
        ];
        cache_file = "/var/lib/dnscrypt-proxy2/public-resolvers.md";
        minisign_key =
          "RWQf6LRCGA9i53mlYecO4IzT51TGPpvWucNSCh1CBM0QTaLn73Y7GFO3";
      };

      server_names = [ "cloudflare" ];
    };
  };

  services.printing.enable = true;
  services.printing.drivers = with pkgs; [ brlaser ];

  services.avahi = {
    enable = true;
    openFirewall = true;
    nssmdns = true;
    publish = {
      enable = true;
      domain = false;
      userServices = true;
    };
  };

  services.invidious.enable = true;

  services.murmur = {
    enable = true;
    openFirewall = true;
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
      "cdrom"
      "audio"
      "wheel"
      "libvirtd"
      "networkmanager"
      "scanner"
      "lp"
      "adbusers"
      "docker"
      "wireshark"
    ];
    shell = pkgs.zsh;
  };

  services.locate = {
    enable = true;
    package = pkgs.mlocate;
    localuser = null; # for pkgs.mlocate
    interval = "hourly"; # fine with mlocate?
    pruneNames = [ ".snapshots" ];
  };

  environment.systemPackages = (with pkgs; [
    wget
    vim
    tmux
    efibootmgr
    unzip
    inetutils
    usbutils # lsusb
    man-pages

    virt-manager
    dconf # needed for saving settings in virt-manager
    libguestfs # needed for virt-sparsify

    virtiofsd

    # packages using udev rules
    antimicrox
  ]);

  boot.extraModulePackages = [ config.boot.kernelPackages.gcadapter-oc-kmod ];
  # to autoload at boot:
  boot.kernelModules = [ "gcadapter_oc" ];
  services.udev.packages =
    [ pkgs.qmk-udev-rules pkgs.antimicrox pkgs.dolphinEmu ];

  services.udev.extraRules = ''
    ACTION=="add", ATTR{idVendor}=="26ce", ATTR{idProduct}=="01a2", RUN="${pkgs.bash}/bin/bash -c 'echo 0 >/sys/\$devpath/authorized'"
    ACTION=="add", ATTR{idVendor}=="1b1c", ATTR{idProduct}=="0c1a", RUN="${pkgs.bash}/bin/bash -c 'echo 0 >/sys/\$devpath/authorized'"

    ACTION=="add", ATTR{idVendor}=="2dc8", ATTR{idProduct}=="3106", RUN="${pkgs.kmod}/bin/modprobe xpad", RUN+="${pkgs.bash}/bin/bash -c 'echo 2dc8 3106 > /sys/bus/usb/drivers/xpad/new_id'"

    ATTRS{idVendor}=="057e", ATTRS{idProduct}=="0337", MODE="666", SUBSYSTEM=="usb", ENV{DEVTYPE}=="usb_device" TAG+="uaccess"
  ''; # last entry is gcc

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

  programs.corectrl = {
    enable = true;
    gpuOverclock.enable = true;
  };

  # List services that you want to enable:

  security.polkit.enable = true;

  virtualisation.libvirtd.enable = true;
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "btrfs"; # NOTE: system specific?
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
  programs.dconf.enable = true;

  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;
  services.blueman.enable = true;

  hardware.sane.enable = true;
  hardware.sane.extraBackends = [ pkgs.epkowa ];

  hardware.opengl = {
    driSupport = true;
    driSupport32Bit = true;
    extraPackages = [ pkgs.rocm-opencl-icd ];
  };

  hardware.steam-hardware.enable = true;

  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
    settings = {
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
      substituters = [ "https://nix-community.cachix.org" ];
      auto-optimise-store = true;
    };
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

  documentation.dev.enable = true;
  documentation.man.generateCaches = true;
}
