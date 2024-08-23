/****************************************************************************
  * Thelxinoe hardware configuration
  *
  **************************************************************************/
{ lib, pkgs, ... }:
{
  boot.loader.grub.useOSProber = true;
  boot.initrd.availableKernelModules = [
    "nvme"
    "xhci_pci"
    "ahci"
    "usbhid"
    "usb_storage"
    "sd_mod"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [
    "kvm-amd"
    "amdgpu"
    "algif_hash"
    "algif_skcipher"
  ];
  boot.extraModulePackages = [ ];

  hardware.enableRedistributableFirmware = true;
  hardware.firmware = [
    pkgs.rtl8761b-firmware
  ];

  hardware.cpu.amd.updateMicrocode = true;

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/e36a4aab-f2fd-4789-ae28-b6eb714b7d85";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/47D3-2028";
    fsType = "vfat";
  };

  fileSystems."/mnt/raid" = {
    device = "fractal:/mnt/raid1";
    fsType = "nfs";
  };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/5fbea3ce-b59a-40de-9330-9d4e0264c8f0"; }];

  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
  # High-DPI console
  console.font =
    lib.mkDefault "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";

  nixpkgs.hostPlatform = "x86_64-linux";
}
