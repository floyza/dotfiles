{ config, osConfig, pkgs, lib, secrets, ... }:

let cfg = osConfig.my.customData;
in {
  imports = [ ../modules/home/zsh ../modules/home/sway ];
  # CHG (trim unnessesary stuff)
  home.packages = with pkgs; [
    (appimage-run.override { extraPkgs = p: [ p.gmpxx ]; })

    tcpdump
    wireshark

    filezilla
    kiwix

    gtypist
    pandoc
    golly
    qjackctl
    ntfs3g
    fuse
    file
    ffmpeg
    wineWowPackages.staging
    winetricks
    protontricks
    libreoffice
    tldr
    libnotify
    units
    graphviz
    nmap
    whipper
    imagemagick
    yt-dlp
    mpv
    wl-mirror
    jq
    sdcv
    easyeffects
    # magic-wormhole
    gnome.nautilus
    anki

    mpc_cli
    killall
    alsa-utils
    gparted
    pavucontrol
    zip
    gotop
    nethogs

    xdg-utils

    krita
    ### programming
    gdb
    ## java
    openjdk17
    ## rust
    rustup
    # cargo
    # rustc
    # rustfmt
    # rust-analyzer
    ## haskell
    haskellPackages.stack
    haskellPackages.haskell-language-server
    haskellPackages.hoogle
    haskellPackages.cabal-install
    haskellPackages.ghc
    ## lisp
    guile
    sbcl
    lispPackages.clwrapper
    ## c++
    ccls
    # lua+fennel
    lua
    sumneko-lua-language-server
    fennel
    fnlfmt
    ## web
    nodePackages.eslint
    nodejs
    ## misc-lang
    odin
    zig
    zls
    ## formatting
    nodePackages.prettier
    shellcheck
    nixfmt
    html-tidy
    ## resources
    # aseprite-unfree
    ### games
    mudlet
    libstrangle
    prismlauncher
    ivan
    endless-sky
    tome4
    # (factorio.override {
    #   username = "gdown";
    #   token = (import secrets).factorio-token;
    # })
    steam
    steam-run-native
    runelite
    lutris
    mangohud
    dolphin-emu-beta
    cataclysm-dda
    crawl
    angband
    nethack
    sil-q
    hyperrogue
    cockatrice
    ### other
    libqalculate
    discord
    element-desktop
    mumble
    murmur
    ## fonts
    emacs-all-the-icons-fonts
    jetbrains-mono
    ### doom dependencies
    gcc # need for emacsql-sqlite
    gnumake
    cmake
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
    (python3.withPackages (ps: with ps; [ sh python-lsp-server ]))
  ];

  programs.man.generateCaches = true;

  programs.password-store = { enable = true; };

  programs.tmux = {
    enable = true;
    escapeTime = 0;
    baseIndex = 1;
    keyMode = "vi";
    shortcut = "z";
    terminal = "xterm-256color";
  };

  # CHG has home dir hardcoded, plus music dir might be different
  programs.beets = {
    enable = true;
    settings = {
      directory = "/home/gavin/mnt/music";
      library = "/home/gavin/mnt/music/library.db";
      plugins = "fetchart";
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
    server.enable = false;
    settings = {
      main = {
        term = "xterm-256color";
        font = "JetBrains Mono:size=10.0";
        # font = "Monospace:size=10";
        bold-text-in-bright = "palette-based";
        initial-window-size-chars = "80x25";
      };
      colors = {
        regular0 = "000000";
        regular1 = "aa0000";
        regular2 = "00aa00";
        regular3 = "aaaa00";
        regular4 = "0000aa";
        regular5 = "aa00aa";
        regular6 = "00aaaa";
        regular7 = "aaaaaa";
        bright0 = "555555";
        bright1 = "ff5555";
        bright2 = "55ff55";
        bright3 = "ffff55";
        bright4 = "5555ff";
        bright5 = "ff55ff";
        bright6 = "55ffff";
        bright7 = "ffffff";
        foreground = "aaaaaa";
        background = "000000";
      };
    };
  };

  gtk = {
    enable = true;
    iconTheme = {
      package = pkgs.gnome.adwaita-icon-theme;
      name = "Adwaita";
    };
    theme = {
      package = pkgs.gnome.gnome-themes-extra;
      name = "Adwaita";
    };
  };

  programs.nix-index.enable = true;

  programs.firefox = {
    enable = true;
    profiles.main = {
      extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        ublock-origin
        greasemonkey
        redirector
        tree-style-tab
      ];
      settings = {
        "trr.mode" = 5; # disable builtin dns-over-https
        "layout.frame_rate" = cfg.primaryOutput.fps;
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
    package = pkgs.gitAndTools.gitFull;
    userEmail = "gavin.downard@runbox.com";
    userName = "Gavin Downard";
    extraConfig = {
      github.user = "floyza";
      sendemail.identity = "runbox";
    };
    ignores = [ ".direnv/" ];
  };

  programs.ssh.enable = true;

  services.mpd = {
    enable = true;
    musicDirectory = "/home/gavin/mnt/music"; # CHG we need a music dir
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

      input {
          plugin "cdio_paranoia"
          enabled "yes"
          speed "1"
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
      {
        key = "l";
        command = [ "next_column" "slave_screen" ];
      }
      {
        key = "h";
        command = [ "previous_column" "master_screen" ];
      }
      {
        key = "'";
        command = [ "show_lyrics" ];
      }
    ];
    settings = {
      user_interface = "alternative";
      visualizer_data_source = "/tmp/mpd.fifo";
      visualizer_output_name = "my_fifo";
      visualizer_in_stereo = "yes";
      visualizer_type = "spectrum";
      visualizer_look = "+|";
      visualizer_fps = cfg.primaryOutput.fps;
      lyrics_fetchers =
        "musixmatch, genius, azlyrics, sing365, metrolyrics, justsomelyrics, jahlyrics, plyrics, tekstowo, zeneszoveg, internet";
    };
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacsUnstable;
    extraPackages = epkgs: [ epkgs.vterm ];
  };

  xdg = {
    enable = true;
    mimeApps.enable = true;
    mimeApps.defaultApplications = {
      "text/plain" = "emacs.desktop";
      "text/org" = "emacs.desktop";
    };
    configFile."mimeapps.list".force = true;
  };

  home.sessionVariables = {
    EDITOR = "emacs -nw";
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
    ".config/gdb/gdbinit".text = ''
      set history save on
    '';
    ".doom.d".source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/src/dotfiles/doom.d"; # NOTE: based on location of repo
  };

  programs.home-manager.enable = true;

  home.username = "gavin";
  home.homeDirectory = "/home/gavin";

  home.stateVersion = "21.11";
}
