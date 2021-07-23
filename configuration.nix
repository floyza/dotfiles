{ config, pkgs, lib, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  time.timeZone = "America/Los_Angeles";

  nixpkgs.config.allowUnfree = true;

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking = {
    hostName = "gavin-nixos";
    networkmanager.enable = true;
    enableIPv6 = false; # vpn might leak otherwise

    useDHCP = false;
    interfaces.enp7s0.useDHCP = true;
    # firewall.allowedTCPPorts = [ ... ];
    # firewall.allowedUDPPorts = [ ... ];
    firewall.enable = false;
  };

  # programs.firejail = {
  #   enable = true;
  #   wrappedBinaries = {
  #     steam = {
  #       executable = "${lib.getBin pkgs.steam}/bin/steam";
  #       profile = "${pkgs.firejail}/etc/firejail/steam.profile";
  #     };
  #   };
  # };

  # services.xserver = {
  #   enable = true;
  #   displayManager.sddm.enable = true;
  #   desktopManager = {
  #     xfce = {
  #       enable = true;
  #       noDesktop = true;
  #       enableXfwm = false;
  #     };
  #   };
  #   windowManager.i3.enable = true;
  #   windowManager.i3.extraPackages = with pkgs; [
  #     feh
  #   ];
  #   displayManager.defaultSession = "xfce+i3";
  # };
  services.xserver = {
    enable = true;
    displayManager.sddm.enable = true;
    desktopManager.plasma5.enable = true;
  };

  # programs.sway = {
  #   enable = true;
  #   wrapperFeatures.gtk = true;
  #   extraPackages = with pkgs; [
  #     swaylock
  #     swayidle
  #     wl-clipboard
  #     mako
  #     alacritty
  #     dmenu
  #   ];
  #   extraSessionCommands = "exec /run/current-system/sw/libexec/polkit-gnome-authentication-agent-1 &";
  # };

  # services.greetd = {
  #   enable = true;
  #   restart = true;
  #   vt = 7;
  #   settings.default_session = {
  #     command = "${pkgs.greetd.tuigreet}/bin/tuigreet -c sway";
  #   };
  # };

  # xdg = { autostart.enable = true; };

  #sound.enable = true; # misleading: enables alsa
  #hardware.pulseaudio.enable = true;
  #hardware.pulseaudio.support32Bit = true;
  #hardware.pulseaudio.package = pkgs.pulseaudioFull;
  # Enable pipewire
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # jack.enable = true;

    # config.pipewire = {
    #   "context.properties" = {
    #     "link.max-buffers" = 64;
    #     "default.clock.rate" = 48000;
    #   };
    # };
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  users.users.gavin = {
    isNormalUser = true;
    extraGroups =
      [ "audio" "wheel" "libvirtd" "networkmanager" "scanner" "lp" "adbusers" ];
    shell = pkgs.zsh;
  };

  hardware.sane = {
    enable = true;
    extraBackends = [ pkgs.epkowa ];
  };

  services.locate.enable = true;

  services.syncthing = {
    enable = true;
    user = "gavin";
    configDir = "/home/gavin/.config/syncthing";
    dataDir = "/home/gavin/syncthing";
    declarative = {
      devices = {
        "home-desktop" = {
          # this is our system
          id =
            "DPHVIJC-2UI6GXX-D2P4TPD-EKYT6RI-VGHQMZE-B6XD5WR-QJC64F4-E75BUAR";
          introducer = true;
        };
      };
      folders = {
        "org" = {
          path = "/home/gavin/syncthing/org";
          devices = [ "home-desktop" ];
        };
      };
    };
  };

  services.openvpn.servers = {
    tcp = {
      config = "config /root/nixos/openvpn/us8272.nordvpn.com.tcp.conf ";
    };
  };

  environment.systemPackages = (with pkgs; [
    wget
    vim
    tmux
    efibootmgr
    unzip
    usbutils # lsusb
    # polkit
    # polkit_gnome
    virt-manager
    # Temporary fix for kde plasma and pipewire
    # kmix
    # plasma-pa
  ]); # ++ (with pkgs.plasma5Packages; [
  #   gwenview
  # ]);

  # require modification to udev rules
  programs.tilp2.enable = true;
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
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.forwardX11 = true;

  security.polkit.enable = true;

  virtualisation.libvirtd.enable = true;
  programs.dconf.enable = true;

  hardware.opengl = {
    driSupport = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [ rocm-opencl-icd rocm-opencl-runtime ];
  };

  nix = {
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

  documentation.info.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}
