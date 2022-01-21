{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ wl-clipboard ];

  services.swayidle = {
    enable = true;
    timeouts = [{
      timeout = 300;
      command = "${pkgs.swaylock}/bin/swaylock -fF -c 1D2021";
    }];
  };

  programs.waybar = {
    enable = true;
    style = builtins.readFile ./waybar-style.css;
    # style = ''
    #   window#waybar {
    #     background-color: rgba(40, 40, 40, .8);
    #   }
    # '';
    settings = [{
      position = "bottom";
      modules-left = [ "sway/workspaces" "sway/mode" "wlr/taskbar" ];
      modules-center = [ "sway/window" ];
      modules-right = [ "cpu" "memory" "clock" "pulseaudio" "mpd" ];
      modules.clock.format = "{:%H:%M}";
    }];
  };

  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    config = let
      output-primary = "DP-3";
      output-secondary = "HDMI-A-1";
    in {
      bars = [{
        command = "${pkgs.waybar}/bin/waybar";
        fonts = { };
      }];
      keybindings = let
        modifier = config.wayland.windowManager.sway.config.modifier;
        pactl = "${pkgs.pulseaudio}/bin/pactl";
        slurp = "${pkgs.slurp}/bin/slurp";
        grim = "${pkgs.grim}/bin/grim";
        date = "${pkgs.coreutils}/bin/date";
        dmenu-bin = "${pkgs.dmenu}/bin";
        jq = "${pkgs.jq}/bin/jq";
      in lib.mkOptionDefault {
        "XF86AudioRaiseVolume" = "exec ${pactl} set-sink-volume 0 +5%";
        "XF86AudioLowerVolume" = "exec ${pactl} set-sink-volume 0 -5%";
        "XF86AudioMute" = "exec ${pactl} set-sink-mute 0 toggle";
        "${modifier}+Shift+a" =
          "exec ${pactl} set-default-sink alsa_output.pci-0000_09_00.4.analog-stereo";
        "${modifier}+Shift+s" =
          "exec ${pactl} set-default-sink alsa_output.usb-Burr-Brown_from_TI_USB_Audio_DAC-00.iec958-stereo";
        "${modifier}+Shift+d" = ''
          exec 'for id in $(pw-dump | ${jq} '"'"'.[] | select(.props."metadata.name" == "default") | .metadata | .[] | select (.key == "target.node") | .subject'"'"'); do pw-metadata -d $id target.node ; done'
        '';
        "${modifier}+d" =
          "exec ${dmenu-bin}/dmenu_path | ${dmenu-bin}/dmenu | zsh -i -s";
        "${modifier}+Tab" = "workspace back_and_forth";
        "${modifier}+e" = "exec emacsclient -c";
        "${modifier}+p" = "exec mpc toggle";
        "${modifier}+i" =
          "exec ${grim} -t png ~/docs/screenshots/$(${date} +%Y-%m-%d_%H-%m-%s).png";
        "${modifier}+Shift+i" = ''
          exec ${grim} -t png -g "$(${slurp})" ~/docs/screenshots/$(${date} +%Y-%m-%d_%H-%m-%s).png'';
      };
      input."*" = {
        accel_profile = "flat";
        pointer_accel = "1";
        xkb_layout = "us";
      };
      output = let background = ./background.jpg;
      in {
        "*" = { bg = "${background} fill"; };
        "${output-primary}" = {
          mode = "1920x1080@144.001Hz";
          bg = "${background} fill";
          pos = "1920 0";
        };
        "${output-secondary}" = {
          mode = "1920x1080@144.001Hz";
          bg = "${background} fill";
          pos = "0 0";
        };
      };
      workspaceOutputAssign = map (name: {
        output = output-primary;
        workspace = name;
      }) [ "1" "2" "3" "4" "5" ] ++ map (name: {
        output = output-secondary;
        workspace = name;
      }) [ "6" "7" "8" "9" ];
      startup = [
        { command = "${pkgs.mako}/bin/mako"; }
        {
          command =
            "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
        }
      ];
      terminal = "footclient";
      modifier = "Mod4"; # super
    };
  };
}
