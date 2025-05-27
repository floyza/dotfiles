{
  config,
  osConfig,
  pkgs,
  lib,
  secrets,
  ...
}:

let
  cfg = osConfig.my.customData;
in
{
  imports = [
    ../modules/home/zsh
    ../modules/home/sway
  ];
  home.packages = with pkgs; [
    (appimage-run.override {
      extraPkgs = p: [
        p.gmpxx
        p.libnotify
        p.pcre2
      ];
    })

    tcpdump

    filezilla

    atool
    samba # smbclient
    qjackctl
    ntfs3g
    file
    ffmpeg
    tldr
    libnotify
    units
    graphviz
    nmap
    imagemagick
    yt-dlp
    mpv
    wl-mirror
    jq
    nautilus
    spotify

    killall
    alsa-utils
    gparted
    pavucontrol
    zip
    gotop
    nethogs

    xdg-utils

    ### programming
    gdb
    ## formatting
    shellcheck
    ### games
    libstrangle
    steam
    steam-run-native
    (lutris.override { extraLibraries = pkgs: [ xorg.libXScrnSaver ]; })
    cataclysm-dda
    crawl
    angband
    nethack
    sil-q
    ### other
    libqalculate
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
    (aspellWithDicts (
      dicts: with dicts; [
        en
        en-computers
        en-science
      ]
    ))
    ## org-mode
    sqlite
    texlive.combined.scheme-full
    texlab
    (python3.withPackages (
      ps: with ps; [
        jupyter
        sh
        python-lsp-server
        pandas
      ]
    ))
    omnisharp-roslyn
    dotnet-sdk

    discord
  ];

  programs.man.generateCaches = true;

  programs.password-store = {
    enable = true;
  };

  programs.tmux = {
    enable = true;
    escapeTime = 0;
    baseIndex = 1;
    keyMode = "vi";
    shortcut = "z";
    terminal = "xterm-256color";
  };

  programs.mbsync.enable = true;
  programs.mu.enable = true;
  accounts.email.maildirBasePath = ".mail";
  accounts.email.accounts.runbox = {
    address = "me@floyza.com";
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
        font = "JetBrains Mono:size=11.0";
        # font = "Monospace:size=10";
        bold-text-in-bright = "palette-based";
        initial-window-size-chars = "80x25";
      };
      colors = {
        # gruvbox
        background = "1d2021"; # 282828
        foreground = "ebdbb2";
        regular0 = "1d2021"; # 282828
        regular1 = "cc241d";
        regular2 = "98971a";
        regular3 = "d79921";
        regular4 = "458588";
        regular5 = "b16286";
        regular6 = "689d6a";
        regular7 = "a89984";
        bright0 = "928374";
        bright1 = "fb4934";
        bright2 = "b8bb26";
        bright3 = "fabd2f";
        bright4 = "83a598";
        bright5 = "d3869b";
        bright6 = "8ec07c";
        bright7 = "ebdbb2";
      };
    };
  };

  home.pointerCursor = {
    enable = true;
    package = pkgs.gnome-themes-extra;
    name = "Adwaita";
    size = 24;
    gtk.enable = true;
    sway.enable = true;
  };

  gtk = {
    enable = true;
    iconTheme = {
      package = pkgs.adwaita-icon-theme;
      name = "Adwaita";
    };
    theme = {
      package = pkgs.gnome-themes-extra;
      name = "Adwaita";
    };
  };

  programs.firefox = {
    enable = true;
    profiles.main = {
      extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [
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
        "privacy.resistFingerprinting" = false; # disables canvas when enabled, breaks chessable
        "privacy.trackingprotection.fingerprinting.enabled" = true; # default
        "privacy.trackingprotection.cryptomining.enabled" = true; # default
        "privacy.trackingprotection.enabled" = true;
        "browser.send_pings" = false; # default
        "media.navigator.enabled" = false; # track mic and camera status
        "network.cookie.cookieBehavior" = 1; # do not accept third-party cookies
        "beacon.enabled" = false;
        "browser.safebrowsing.downloads.remote.enabled" = false; # don't integrate with Google Safe Browsing
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
    userEmail = "me@floyza.com";
    userName = "floyza";
    extraConfig = {
      github.user = "floyza";
      sendemail.identity = "runbox";
    };
    ignores = [ ".direnv/" ];
  };

  programs.ssh.enable = true;

  services.mpd = {
    enable = true;
    musicDirectory = cfg.musicDirectory;
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

  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    extraPackages = epkgs: [
      epkgs.vterm
      epkgs.mu4e
    ];
  };

  xdg = {
    enable = true;
    mimeApps.enable = true;
    mimeApps.defaultApplications = {
      "text/plain" = "emacs.desktop";
      "text/org" = "emacs.desktop";
      "text/html" = "firefox.desktop";
      "x-scheme-handler/http" = "firefox.desktop";
      "x-scheme-handler/https" = "firefox.desktop";
      "x-scheme-handler/about" = "firefox.desktop";
      "x-scheme-handler/unknown" = "firefox.desktop";
    };
    configFile."mimeapps.list".force = true;
  };

  home.sessionVariables = {
    GTK_IM_MODULE = "xim"; # lets XCompose work
    EDITOR = "vim";
    DEFAULT_BROWSER = "${pkgs.firefox}/bin/firefox";
    # BUG Plugin paths are not automatically added, so we must add them
    DSSI_PATH = "$HOME/.dssi:$HOME/.nix-profile/lib/dssi:/run/current-system/sw/lib/dssi";
    LADSPA_PATH = "$HOME/.ladspa:$HOME/.nix-profile/lib/ladspa:/run/current-system/sw/lib/ladspa";
    LV2_PATH = "$HOME/.lv2:$HOME/.nix-profile/lib/lv2:/run/current-system/sw/lib/lv2";
    LXVST_PATH = "$HOME/.lxvst:$HOME/.nix-profile/lib/lxvst:/run/current-system/sw/lib/lxvst";
    VST_PATH = "$HOME/.vst:$HOME/.nix-profile/lib/vst:/run/current-system/sw/lib/vst";
    VST3_PATH = "$HOME/.vst3:$HOME/.nix-profile/lib/vst3:/run/current-system/sw/lib/vst3";
  };

  home.sessionPath = [ "$HOME/bin" ];

  home.file = {
    ".sbclrc".text = ''
      (require 'asdf)
      (push '*default-pathname-defaults* asdf:*central-registry*)
    '';
    ".XCompose".text = ''
      include "%L"
      <Multi_key> <s> <h> <r> <u> <g> : "¯\\_(ツ)_/¯"
    '';
  };
  xdg.configFile = {
    "gdb/gdbinit".text = ''
      set history save on
    '';
    doom.source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/src/dotfiles/doom"; # NOTE: based on location of repo
  };

  programs.home-manager.enable = true;

  home.username = "gavin";
  home.homeDirectory = "/home/gavin";
}
