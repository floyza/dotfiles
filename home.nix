{ config, pkgs, lib, ... }:

let
  tome4-latest = pkgs.tome4.overrideAttrs (oldAttrs: rec {
    version = "1.7.4";
    src = pkgs.fetchurl {
      url = "https://te4.org/dl/t-engine/t-engine4-src-${version}.tar.bz2";
      sha256 = "sha256-w1NPM/SMnPAnAl6z9E6Xsj3mEqZtXzFe1IMPmlKr8qQ=";
    };
    installPhase = let
      pname = "tome4";
      desktop = pkgs.makeDesktopItem {
        desktopName = pname;
        name = pname;
        exec = "@out@/bin/${pname}";
        icon = pname;
        terminal = "false";
        comment =
          "An open-source, single-player, role-playing roguelike game set in the world of Eyal.";
        type = "Application";
        categories = "Game;RolePlaying;";
        genericName = pname;
      };
    in ''
      runHook preInstall
      dir=$out/share/${pname}
      install -Dm755 t-engine $dir/t-engine
      cp -r bootstrap game $dir
      makeWrapper $dir/t-engine $out/bin/${pname} \
        --run "cd $dir"
      install -Dm755 ${desktop}/share/applications/${pname}.desktop $out/share/applications/${pname}.desktop
      substituteInPlace $out/share/applications/${pname}.desktop \
        --subst-var out
      unzip -oj -qq game/engines/te4-${version}.teae data/gfx/te4-icon.png
      install -Dm644 te4-icon.png $out/share/icons/hicolor/64x64/${pname}.png
      install -Dm644 -t $out/share/doc/${pname} CONTRIBUTING COPYING COPYING-MEDIA CREDITS
      runHook postInstall
    '';
  });
in {
  home.packages = with pkgs; [
    lutris
    mpd
    mpc_cli
    killall
    alsaUtils
    #ardour
    gparted
    pavucontrol
    zip
    # distrho # vitalium
    # zynaddsubfx
    #godot
    gotop
    #torbrowser
    nyxt
    #krita
    ## programming
    guile
    sbcl
    lispPackages.clwrapper
    ## games
    steam
    steam-run-native
    cataclysm-dda
    cataclysmDDA.stable.curses
    #runelite
    crawl
    angband
    hyperrogue
    cockatrice
    tome4-latest
    #zeroad
    #factorio
    ## other
    calc
    discord
    element-desktop
    ## fonts
    emacs-all-the-icons-fonts
    jetbrains-mono
    # doom dependencies
    gcc # need for emacsql-sqlite
    fd
    ripgrep
    gnutls
    # optional doom
    zstd
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    # ispell
    # org-mode
    sqlite
    texlive.combined.scheme-medium
    (python38.withPackages (ps: with ps; [ jupyter python-language-server ]))
    ## programming tools (lsp/linter/fmt)
    # hard deps
    nixfmt
    # soft deps
    shellcheck
  ];

  programs.password-store = { enable = true; };

  programs.beets = {
    enable = true;
    settings = {
      directory = "/home/gavin/Music";
      #directory = "/mnt/music/beets";
      library = "/home/gavin/.config/beets/musiclibrary.blb";
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
        "browser.startup.homepage" = "https://duckduckgo.com";
        # see https://privacytools.io/browsers/#about_config
        "privacy.firstparty.isolate" = true;
        "privacy.resistFingerprinting" = true;
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

  #programs.emacs = ({
  #  enable = true;
  #  package = emacs;
  #} // (if !use-nix-doom then {
  #  extraPackages = epkgs: [ epkgs.vterm ];
  #} else
  #  { }));

  programs.doom-emacs = {
    enable = true;
    doomPrivateDir = ./doom.d;
  };

  xsession.windowManager.i3 = {
    enable = true;
    config = {
      terminal = "xfce4-terminal";
      modifier = "Mod4"; # super (called meta in i3 docs)
      keybindings = let
        mod = config.xsession.windowManager.i3.config.modifier;
        amixer = "${pkgs.alsaUtils}/bin/amixer";
      in lib.mkOptionDefault {
        "${mod}+Tab" = "workspace back_and_forth";
        "${mod}+Control+e" = "exec emacsclient -c";
        # "${mod}+space" =
        "${mod}+g" = "split h";
        "${mod}+h" = "focus left";
        "${mod}+j" = "focus down";
        "${mod}+k" = "focus up";
        "${mod}+l" = "focus right";
        "XF86AudioRaiseVolume" = "exec ${amixer} set Master 5%+ -M";
        "XF86AudioLowerVolume" = "exec ${amixer} set Master 5%- -M";
        "XF86AudioMute" = "exec ${amixer} set Master toggle";
      };
      startup = [
        {
          command = "firefox";
          workspace = "1:web";
        }
        {
          command = "emacsclient -c";
          workspace = "2:coding";
        }
        { command = "feh --bg-scale ${./nord-bg.png}"; }
      ];
    };
  };

  # wayland.windowManager.sway = {
  #   enable = true;
  #   wrapperFeatures.gtk = true;
  #   config = {
  #     keybindings = let
  #       modifier = config.wayland.windowManager.sway.config.modifier;
  #       amixer = "${pkgs.alsaUtils}/bin/amixer";
  #     in lib.mkOptionDefault {
  #       "XF86AudioRaiseVolume" = "exec ${amixer} set Master 5%+ -M";
  #       "XF86AudioLowerVolume" = "exec ${amixer} set Master 5%- -M";
  #       "XF86AudioMute" = "exec ${amixer} set Master toggle";
  #       "${modifier}+Tab" = "workspace back_and_forth";
  #       "${modifier}+e" = "exec emacsclient -c";
  #     };
  #     input."*" = { pointer_accel = "0"; };
  #     output = {
  #       "*" = { bg = "${./gruvbox-dark-blue.png} fill"; };
  #       DP-3 = {
  #         mode = "1920x1080@144.001Hz";
  #         bg = "${./gruvbox-dark-blue.png} fill";
  #       };
  #     };
  #     startup = [{ command = "mako"; }];
  #     terminal = "alacritty";
  #     modifier = "Mod4"; # super
  #   };
  # };

  # services.udiskie = {
  #   enable = true;
  #   tray = "never";
  # };

  ## kde takes care of this
  # gtk = {
  #   enable = true;
  #   iconTheme = {
  #     package = pkgs.gruvbox-dark-gtk;
  #     name = "gruvbox-dark-gtk";
  #   };
  #   theme = {
  #     package = pkgs.gruvbox-dark-gtk;
  #     name = "gruvbox-dark-gtk";
  #   };
  # };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    autocd = true;
    defaultKeymap = "emacs";
    plugins = (with pkgs; [
      {
        name = "zsh-syntax-highlighting";
        src = zsh-syntax-highlighting;
      }
      {
        name = "zsh-powerlevel10k";
        src = zsh-powerlevel10k;
      }
      {
        name = "zsh-history-substring-search";
        src = zsh-history-substring-search;
      }
    ]);
    initExtra = ''
      if [[ "$INSIDE_EMACS" = 'vterm' ]] \
         && [[ -n "$EMACS_VTERM_PATH" ]] \
            && [[ -f "$EMACS_VTERM_PATH"/etc/emacs-vterm-zsh.sh ]]; then
               source "$EMACS_VTERM_PATH"/etc/emacs-vterm-zsh.sh
      fi
      nrun() {
             nix-shell -p "$1" --run "$1"
      }
      weather () {
              curl https://wttr.in/"$1"
      }
    '';
    shellAliases = { octal = "stat -c '%a %n'"; };
  };

  programs.git = {
    enable = true;
    userEmail = "gavin.downard@runbox.com";
    userName = "Gavin Downard";
  };

  programs.ssh.enable = true;

  services.mpd = {
    enable = true;
    musicDirectory = "/home/gavin/Music";
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

  home.sessionVariables = {
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

  home.file = {
      # ".config/common-lisp/source-registry.conf.d/50-luser.lisp.conf".text = ''
      #   (:tree "$HOME/src/my/lisp/")
      # '';
      ".sbclrc".text = ''
        (require 'asdf)
        (push '*default-pathname-defaults* asdf:*central-registry*)
      '';
    };
  services.emacs.enable = true;

  programs.home-manager.enable = true;

  home.username = "gavin";
  home.homeDirectory = "/home/gavin";

  home.stateVersion = "21.03";
}
