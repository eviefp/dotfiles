/****************************************************************************
  * Arche hardware configuration
  *
  **************************************************************************/
{ lib, pkgs, ... }:
{
  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "ahci"
    "usb_storage"
    "usbhid"
    "sd_mod"
  ];
  boot.blacklistedKernelModules = [ "nouveau" "nvidiafb" ];
  boot.kernelModules = [
    "kvm-intel"
    "nvidia"
    "nvidia_modeset"
    "nvidia_uvm"
    "nvidia_drm"
    "alif_hash"
    "algif_skcipher"
  ];

  hardware.enableRedistributableFirmware = true;
  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.cpu.intel.updateMicrocode = true;

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/ESP";
    fsType = "vfat";
  };

  swapDevices = [
    { device = "/dev/disk/by-label/swap"; }
  ];


  nix.settings.max-jobs = lib.mkDefault 8;
  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
  console.font =
    lib.mkDefault "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";
  nixpkgs.hostPlatform = "x86_64-linux";
}
