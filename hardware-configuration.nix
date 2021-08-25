# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot.initrd.availableKernelModules =
    [ "nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/4a964a46-1ff2-486d-92d2-59095b020e26";
      fsType = "ext4";
    };

    "/home/gavin/mnt" = {
      device = "/dev/disk/by-uuid/a9abd67e-faab-4241-82de-60fb2c337005";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/FE84-A8A6";
      fsType = "vfat";
    };
  };

  swapDevices = [ ];

}
