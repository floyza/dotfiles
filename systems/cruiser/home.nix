{ config, lib, pkgs, ... }:

{
  wayland.windowManager.sway.config = {
    input."2:8:AlpsPS/2_ALPS_GlidePoint" = {
      accel_profile = "adaptive";
      pointer_accel = "0.2";
      tap = "enabled";
    };
  };
  home.stateVersion = "24.11";
}
