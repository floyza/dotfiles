{
  config,
  osConfig,
  lib,
  pkgs,
  ...
}:

let
  cfg = osConfig.my.customData;
in
{

  home.packages = with pkgs; [
    (btop.override { rocmSupport = true; })
    ### programming
    ## resources
    godot
    ## rust
    rustup
    ## formatting
    nodePackages.prettier
    nixfmt-rfc-style
    html-tidy
    ## web
    nodePackages.eslint
    nodejs
    # lua+fennel
    lua
    sumneko-lua-language-server
    fennel
    fnlfmt
    ## c++
    clang-tools
    ## lisp
    guile
    ## raku
    rakudo
    zef
    ## haskell
    haskellPackages.stack
    haskellPackages.haskell-language-server
    haskellPackages.hoogle
    haskellPackages.cabal-install
    haskellPackages.ghc
    # cargo
    # rustc
    # rustfmt
    # rust-analyzer
    ## java
    openjdk17
    ## resources
    aseprite
    ### games
    retroarchFull
    obs-studio
    pcsx2
    runelite
    pyfa
    prismlauncher
    mangohud
    dolphin-emu-beta
    hyperrogue
    cockatrice
    mumble

    kiwix
    brave
    chromium
    gtypist
    pandoc
    golly
    fuse
    wineWowPackages.staging
    winetricks
    protontricks
    libreoffice
    whipper
    anki

    mpc_cli
    krita
  ];

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
        command = [
          "select_item"
          "scroll_down"
        ];
      }
      {
        key = "K";
        command = [
          "select_item"
          "scroll_up"
        ];
      }
      {
        key = "l";
        command = [
          "next_column"
          "slave_screen"
        ];
      }
      {
        key = "h";
        command = [
          "previous_column"
          "master_screen"
        ];
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
      lyrics_fetchers = "musixmatch, genius, azlyrics, sing365, metrolyrics, justsomelyrics, jahlyrics, plyrics, tekstowo, zeneszoveg, internet";
    };
  };

  programs.beets = {
    enable = true;
    settings = {
      directory = config.services.mpd.musicDirectory;
      library = "${config.services.mpd.musicDirectory}/library.db";
      plugins = "fetchart";
      fetchart.auto = true;
    };
  };

  programs.nix-index.enable = true;

  home.stateVersion = "21.11";
}
