{ config, pkgs, lib, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  time.timeZone = "America/Los_Angeles";

  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [
    (self: super: {
      steam = super.steam.override { extraPkgs = pkgs: [ pkgs.libpng_apng ]; };
    })
  ];

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking = {
    hostName = "dreadnought";
    networkmanager.enable = true;
    enableIPv6 = false; # vpn might leak otherwise

    useDHCP = false;
    interfaces.enp4s0.useDHCP = true;
    # firewall.allowedTCPPorts = [ ... ];
    # firewall.allowedUDPPorts = [ ... ];
    firewall.enable = false;
  };

  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    extraPackages = with pkgs; [
      swaylock
      swayidle
      wl-clipboard
      mako
      alacritty
      dmenu
    ];
  };

  # Enable pipewire
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  users.users.gavin = {
    isNormalUser = true;
    extraGroups =
      [ "audio" "wheel" "libvirtd" "networkmanager" "scanner" "lp" "adbusers" ];
    shell = pkgs.zsh;
  };

  services.locate = {
    enable = true;
    locate = pkgs.mlocate;
    localuser = null; # for pkgs.mlocate
    interval = "hourly"; # fine with mlocate?
  };

  services.openvpn.servers = {
    tcp = {
      config = "config /root/nixos/openvpn/us8272.nordvpn.com.tcp.conf ";
      autoStart = false;
    };
  };

  environment.systemPackages = (with pkgs; [
    wget
    vim
    tmux
    efibootmgr
    unzip
    usbutils # lsusb

    virt-manager
    gnome3.dconf # needed for saving settings in virt-manager
    libguestfs # needed for virt-sparsify
  ]);

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
  };

  nix = {
    autoOptimiseStore = true;
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
  system.stateVersion = "21.11"; # Did you read the comment?

}
