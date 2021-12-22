{ config, lib, pkgs, ... }:

{
  ssbm = {
    overlay.enable = true;
    cache.enable = true;
    gcc = {
      oc-kmod.enable = true;
      rules.enable = true;
    };
  };
}
