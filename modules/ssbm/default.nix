{ config, lib, pkgs, ... }:

{
  ssbm = {
    overlay.enable = true;
    cache.enable = false;
    gcc = {
      oc-kmod.enable = true;
      rules.enable = true;
    };
  };
}
