/****************************************************************************
  * Thelxinoe hardware configuration
  *
  **************************************************************************/
{ lib, pkgs, ... }:

{
  imports = [ ];

  boot.loader.grub.useOSProber = true;
  boot.initrd.availableKernelModules =
    [ "nvme" "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" "nvidia" "nvidia_modeset" "nvidia_uvm" "nvidia_drm" ];
  boot.extraModulePackages = [ ];

  hardware.enableRedistributableFirmware = true;
  hardware.firmware = [
    pkgs.rtl8761b-firmware
  ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/e36a4aab-f2fd-4789-ae28-b6eb714b7d85";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/47D3-2028";
    fsType = "vfat";
  };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/5fbea3ce-b59a-40de-9330-9d4e0264c8f0"; }];

  nix.settings.max-jobs = lib.mkDefault 24;
  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
  # High-DPI console
  console.font =
    lib.mkDefault "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";
}
