{ config, osConfig, lib, pkgs, ... }:

let cfg = osConfig.my.customData;
in {
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
      position = "top";
      modules-left = [ "sway/workspaces" "sway/mode" "wlr/taskbar" ];
      modules-center = [ "sway/window" ];
      modules-right = [ "cpu" "memory" "clock" "pulseaudio" "mpd" ];
      clock = {
        format = "{:%A, %B %d, %Y - %R}";
        tooltip-format = "<tt>{calendar}</tt>";
        calendar = {
          format = {
            months = "<span color='#ffead3'><b>{}</b></span>";
            days = "<span color='#ecc6d9'><b>{}</b></span>";
            weeks = "<span color='#99ffdd'><b>W{}</b></span>";
            weekdays = "<span color='#ffcc66'><b>{}</b></span>";
            today = "<span color='#ff6699'><b><u>{}</u></b></span>";
          };
        };
      };
      "wlr/taskbar" = {
        on-click = "activate";
        on-click-right = "close";
      };
    }];
  };

  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    config = let modifier = config.wayland.windowManager.sway.config.modifier;
    in {
      bars = [{
        command = "${pkgs.waybar}/bin/waybar";
        fonts = { };
      }];
      window.titlebar = false;
      floating.titlebar = false;
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
        "${modifier}+c" = "exec emacs --eval '(full-calc)'";
        "${modifier}+Shift+a" =
          "exec ${pactl} set-default-sink ${cfg.primaryAudio}";
        "${modifier}+Shift+s" =
          "exec ${pactl} set-default-sink ${cfg.secondaryAudio}";
        "${modifier}+Shift+r" = ''
          exec 'for id in $(pw-dump | ${jq} '"'"'.[] | select(.props."metadata.name" == "default") | .metadata | .[] | select (.key == "target.object") | .subject'"'"'); do pw-metadata -- $id target.node -1 ; pw-metadata -- $id target.object -1 ; done'
        '';
        "${modifier}+d" = "exec ${wofi} --show=drun";
        "${modifier}+Shift+d" = "exec ${wofi} --show=run";
        "${modifier}+Tab" = "workspace back_and_forth";
        "${modifier}+e" = "exec emacs";
        "${modifier}+p" = "exec mpc toggle";
        "${modifier}+i" =
          "exec ${grim} -t png /home/gavin/docs/screenshots/$(${date} +%Y-%m-%d_%H-%m-%s).png";
        "${modifier}+Shift+i" = ''
          exec ${grim} -t png -g "$(${slurp})" ~/docs/screenshots/$(${date} +%Y-%m-%d_%H-%m-%s).png'';
        "${modifier}+m" = "mode passthrough";
      };
      modes = { passthrough = { "${modifier}+m" = "mode default"; }; };
      input."*" = {
        accel_profile = "flat";
        pointer_accel = "0.5";
        xkb_layout = "us";
      };
      output = let background = ./wallpaper.png;
      in {
        "*" = {
          bg = "${background} fill";
          adaptive_sync = "on";
        };
        "${cfg.primaryOutput.id}" = {
          mode = "${cfg.primaryOutput.mode}";
          # pos = "0 0";
          subpixel = "rgb";
        };
      };
      # workspaceOutputAssign = map (name: {
      #   output = output-primary;
      #   workspace = name;
      # }) [ "1" "2" "3" "4" "5" ];
      startup = [
        { command = "${pkgs.mako}/bin/mako"; }
        {
          command =
            "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
        }
        # { command = "fcitx5 -d"; }
        # { command = "show-me.sh"; }
      ];
      terminal = "foot";
      modifier = "Mod4"; # super

      gaps.inner = 5;
    };
  };
}
