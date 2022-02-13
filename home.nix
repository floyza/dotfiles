{ config, pkgs, lib, ... }:

{
  imports = [ ./modules/home/zsh ./modules/home/sway ];
  home.packages = with pkgs; [
    (appimage-run.override { extraPkgs = p: [ p.gmpxx ]; })

    qjackctl
    ntfs3g
    fuse
    file
    ffmpeg
    wineWowPackages.staging
    libreoffice
    tldr
    libnotify
    units
    graphviz
    libsixel
    nmap
    whipper
    imagemagick
    yt-dlp
    mpv
    wl-mirror
    jq
    sdcv
    easyeffects

    mpd
    mpc_cli
    killall
    alsaUtils
    gparted
    pavucontrol
    zip
    gotop
    s-tui

    xdg_utils

    krita
    ### programming
    ## haskell
    haskellPackages.stack
    haskellPackages.haskell-language-server
    haskellPackages.hoogle
    haskellPackages.cabal-install
    haskellPackages.ghc
    haskellPackages.ormolu
    ## lisp
    guile
    sbcl
    lispPackages.clwrapper
    ## c++
    ccls
    ## misc-lang
    odin
    zig
    zls
    ## formatting
    shellcheck
    nixfmt
    ## resources
    godot
    aseprite-unfree
    (callPackage ./packages/reaper { })
    # (callPackage ./packages/aseprite { unfree = true; })
    ### games
    (factorio.override {
      username = "gdown";
      token = "";
    })
    yuzu
    steam
    steam-run-native
    runelite
    (dwarf-fortress-packages.dwarf-fortress-full.override {
      theme = null;
      enableIntro = false;
      enableFPS = true;
      enableTWBT = false;
    })
    lutris
    dolphin-emu-beta
    slippi-netplay
    cataclysm-dda
    cataclysmDDA.stable.curses
    crawl
    (lib.overrideDerivation angband (oldAttrs: rec {
      # buildInputs = [ x11 ] ++ oldAttrs.buildInputs;
      version = "4.2.3-dev";
      src = fetchFromGitHub {
        owner = "angband";
        repo = "angband";
        rev = "ce314075ed78e8fb7fcc6352c4f4bd232b61f2f4";
        sha256 = "sha256-8d2miabaU5pwlCzUrdprJ/OjHgfhrKTvnn8Ll/aIEZE=";
      };
    }))
    nethack
    hyperrogue
    cockatrice
    wesnoth
    retroarchFull
    ### other
    calc
    discord
    element-desktop
    mumble
    murmur
    ## fonts
    emacs-all-the-icons-fonts
    jetbrains-mono
    ### doom dependencies
    gcc # need for emacsql-sqlite
    fd
    ripgrep
    gnutls
    ## optional doom
    zstd
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    ## org-mode
    sqlite
    texlive.combined.scheme-full
    texlab
    (python39.withPackages (ps: with ps; [ sh ]))
    python-language-server
  ];

  programs.man.generateCaches = true;

  programs.password-store = { enable = true; };

  programs.beets = {
    enable = true;
    settings = {
      directory = "/home/gavin/mnt/music";
      library = "/home/gavin/mnt/music/library.db";
      plugins = "fetchart";
      import.move = true;
      fetchart.auto = true;
    };
  };

  programs.mbsync.enable = true;
  programs.mu.enable = true;
  accounts.email.maildirBasePath = ".mail";
  accounts.email.accounts.runbox = {
    address = "gavin.downard@runbox.com";
    userName = "gavin.downard@runbox.com";
    flavor = "runbox.com";
    gpg.key = "223072757DD22529B1DD7039FD434054864A749B";
    primary = true;
    realName = "Gavin Downard";
    passwordCommand = "pass email/runbox.com";
    mu.enable = true;
    mbsync = {
      enable = true;
      create = "maildir";
    };
  };

  programs.foot = {
    enable = true;
    server.enable = true;
    settings = {
      main = {
        term = "xterm-256color";
        font = "JetBrains Mono:pixelsize=13";
        bold-text-in-bright = true;
      };
    };
  };

  gtk = {
    enable = true;
    iconTheme = {
      package = pkgs.gruvbox-dark-icons-gtk;
      name = "gruvbox-dark";
    };
    theme = {
      package = pkgs.gruvbox-dark-gtk;
      name = "gruvbox-dark";
    };
  };

  programs.nix-index.enable = true;

  programs.firefox = {
    enable = true;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      https-everywhere
      ublock-origin
      clearurls
    ];
    profiles.main = {
      settings = {
        "layout.frame_rate" = 144;
        "browser.startup.homepage" = "https://duckduckgo.com";
        # see https://privacytools.io/browsers/#about_config
        "privacy.firstparty.isolate" = true;
        "privacy.resistFingerprinting" =
          false; # disables canvas when enabled, breaks chessable
        "privacy.trackingprotection.fingerprinting.enabled" = true; # default
        "privacy.trackingprotection.cryptomining.enabled" = true; # default
        "privacy.trackingprotection.enabled" = true;
        "browser.send_pings" = false; # default
        "dom.event.clipboardevents.enabled" = false; # track clipboard events
        "media.navigator.enabled" = false; # track mic and camera status
        "network.cookie.cookieBehavior" = 1; # do not accept third-party cookies
        "beacon.enabled" = false;
        "browser.safebrowsing.downloads.remote.enabled" =
          false; # don't integrate with Google Safe Browsing
        "network.dns.disablePrefetch" = true; # default
        "network.dns.disablePrefetchFromHTTPS" = true; # default
        "network.predictor.enabled" = false; # default
        "network.predictor.enable-prefetch" = false; # default
        "network.prefetch-next" = false; # default
        "network.IDN_show_punycode" = true;
      };
    };
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.git = {
    enable = true;
    userEmail = "gavin.downard@runbox.com";
    userName = "Gavin Downard";
    extraConfig = { github.user = "floyza"; };
    ignores = [ ".direnv/" ];
  };

  programs.ssh.enable = true;

  services.mpd = {
    enable = true;
    musicDirectory = "/home/gavin/mnt/music";
    extraConfig = ''
      audio_output {
              type            "pulse"
              name            "pulse audio"
      }
      audio_output {
          type                    "fifo"
          name                    "my_fifo"
          path                    "/tmp/mpd.fifo"
          format                  "44100:16:2"
      }
    '';
  };

  programs.ncmpcpp = {
    enable = true;
    package = pkgs.ncmpcpp.override { visualizerSupport = true; };
    bindings = [
      {
        key = "j";
        command = "scroll_down";
      }
      {
        key = "k";
        command = "scroll_up";
      }
      {
        key = "J";
        command = [ "select_item" "scroll_down" ];
      }
      {
        key = "K";
        command = [ "select_item" "scroll_up" ];
      }
    ];
    settings = {
      user_interface = "alternative";
      visualizer_data_source = "/tmp/mpd.fifo";
      visualizer_output_name = "my_fifo";
      visualizer_in_stereo = "yes";
      visualizer_type = "spectrum";
      visualizer_look = "+|";
    };
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacsPgtkGcc;
    extraPackages = epkgs: [ epkgs.vterm ];
  };

  home.sessionVariables = {
    MOZ_ENABLE_WAYLAND = "1";
    # BUG Plugin paths are not automatically added, so we must add them
    DSSI_PATH =
      "$HOME/.dssi:$HOME/.nix-profile/lib/dssi:/run/current-system/sw/lib/dssi";
    LADSPA_PATH =
      "$HOME/.ladspa:$HOME/.nix-profile/lib/ladspa:/run/current-system/sw/lib/ladspa";
    LV2_PATH =
      "$HOME/.lv2:$HOME/.nix-profile/lib/lv2:/run/current-system/sw/lib/lv2";
    LXVST_PATH =
      "$HOME/.lxvst:$HOME/.nix-profile/lib/lxvst:/run/current-system/sw/lib/lxvst";
    VST_PATH =
      "$HOME/.vst:$HOME/.nix-profile/lib/vst:/run/current-system/sw/lib/vst";
    VST3_PATH =
      "$HOME/.vst3:$HOME/.nix-profile/lib/vst3:/run/current-system/sw/lib/vst3";
  };

  home.sessionPath = [ "$HOME/bin" ];

  home.file = {
    ".sbclrc".text = ''
      (require 'asdf)
      (push '*default-pathname-defaults* asdf:*central-registry*)
    '';
  };
  services.emacs.enable = true;

  programs.home-manager.enable = true;

  home.username = "gavin";
  home.homeDirectory = "/home/gavin";

  home.stateVersion = "21.11";
}
