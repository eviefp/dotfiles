/**
**************************************************************************
* Fractal hardware configuration
*
*************************************************************************
*/
{lib, ...}: {
  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "ahci"
    "nvme"
    "usbhid"
    "usb_storage"
    "sd_mod"
  ];
  boot.kernelParams = ["nomodeset"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-intel"];
  boot.extraModulePackages = [];

  boot.swraid = {
    enable = true;
    mdadmConf = ''
      DEVICE /dev/sda1 /dev/sdb1
      ARRAY /dev/md1 metadata=1.2 name=fractal:1 UUID=da6f6827:de95048d:d6b3eb65:6cd3dac9
    '';
  };

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
  };

  fileSystems."/mnt/raid1" = {
    device = "/dev/md1";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/boot";
    fsType = "vfat";
  };

  swapDevices = [{device = "/dev/disk/by-label/swap";}];

  nix.settings.max-jobs = lib.mkDefault 12;
  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
  hardware.cpu.intel.updateMicrocode = true;
  nixpkgs.hostPlatform = "x86_64-linux";
}
