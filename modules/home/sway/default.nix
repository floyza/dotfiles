{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ wl-clipboard ];

  services.swayidle = {
    enable = false;
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
      modules-right =
        [ "cpu" "memory" "custom/emacs-clock" "clock" "pulseaudio" "mpd" ];
      modules.clock.format = "{:%H:%M}";
      modules."custom/emacs-clock" = {
        exec = ''
          jq -c -n --arg text "$(emacsclient -e '(org-duration-from-minutes (org-clock-get-clocked-time))' | tr -d '"')" ''
          + ''
            --arg class "$(emacsclient -e "(if (org-clocking-p) 'in 'out)")" ''
          + ''
            '{"text": $text, "class": $class, "tooltip": "Emacs clock time"}' '';

        return-type = "json";

        exec-if = "pgrep emacs";
        on-click = "emacsclient -e '(+org/toggle-last-clock nil)'";
        interval = 10;
      };
    }];
  };

  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    config = let
      output-primary = "DP-1";
      output-secondary = "HDMI-A-1";
      modifier = config.wayland.windowManager.sway.config.modifier;
    in {
      bars = [{
        command = "${pkgs.waybar}/bin/waybar";
        fonts = { };
      }];
      keybindings = let
        pactl = "${pkgs.pulseaudio}/bin/pactl";
        slurp = "${pkgs.slurp}/bin/slurp";
        grim = "${pkgs.grim}/bin/grim";
        date = "${pkgs.coreutils}/bin/date";
        wofi = "${pkgs.wofi}/bin/wofi";
        jq = "${pkgs.jq}/bin/jq";
      in lib.mkOptionDefault {
        "XF86AudioRaiseVolume" = "exec ${pactl} set-sink-volume 0 +5%";
        "XF86AudioLowerVolume" = "exec ${pactl} set-sink-volume 0 -5%";
        "XF86AudioMute" = "exec ${pactl} set-sink-mute 0 toggle";
        "${modifier}+c" = "exec emacsclient -c -e '(full-calc)'";
        "${modifier}+Shift+a" =
          "exec ${pactl} set-default-sink alsa_output.usb-C-Media_Electronics_Inc._USB_Audio_Device-00.analog-stereo";
        "${modifier}+Shift+s" =
          "exec ${pactl} set-default-sink alsa_output.usb-Burr-Brown_from_TI_USB_Audio_DAC-00.iec958-stereo";
        "${modifier}+Shift+r" = ''
          exec 'for id in $(pw-dump | ${jq} '"'"'.[] | select(.props."metadata.name" == "default") | .metadata | .[] | select (.key == "target.node") | .subject'"'"'); do pw-metadata -- $id target.node -1 ; done'
        '';
        "${modifier}+d" = "exec ${wofi} --show=drun";
        "${modifier}+Shift+d" = "exec ${wofi} --show=run";
        "${modifier}+Tab" = "workspace back_and_forth";
        "${modifier}+e" = "exec emacsclient -c";
        "${modifier}+p" = "exec mpc toggle";
        "${modifier}+i" =
          "exec ${grim} -t png ~/docs/screenshots/$(${date} +%Y-%m-%d_%H-%m-%s).png";
        "${modifier}+Shift+i" = ''
          exec ${grim} -t png -g "$(${slurp})" ~/docs/screenshots/$(${date} +%Y-%m-%d_%H-%m-%s).png'';
        "${modifier}+m" = "mode passthrough";
        "${modifier}+t" = "output ${output-secondary} toggle";
      };
      modes = { passthrough = { "${modifier}+m" = "mode default"; }; };
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
