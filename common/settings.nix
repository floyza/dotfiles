{ config, lib, pkgs, ... }:

with lib; {
  options.my.customData = {
    # chuck all the system-specific data in here :)
    primaryAudio = mkOption {
      default = "0";
      type = lib.types.str;
      description = "Primary audio sink.";
    };

    secondaryAudio = mkOption {
      default = "0";
      type = lib.types.str;
      description = "Secondary audio sink.";
    };

    primaryOutput.id = mkOption {
      type = lib.types.str;
      description = "Primary display output id.";
    };

    primaryOutput.mode = mkOption {
      type = lib.types.str;
      description = "Primary display mode line.";
    };

    primaryOutput.fps = mkOption {
      type = lib.types.int;
      description = "Primary display refresh rate.";
    };
  };
}
