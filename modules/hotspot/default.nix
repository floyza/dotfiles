{ config, lib, pkgs, ... }:

{
  environment.etc = {
    "NetworkManager/system-connections/Hotspot.nmconnection".source =
      ./Hotspot.nmconnection;
  };
}
