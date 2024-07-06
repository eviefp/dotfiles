/****************************************************************************
  * Fractal hardware configuration
  *
  **************************************************************************/
{ config, lib, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot.initrd.availableKernelModules =
    [ "xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

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

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/boot";
    fsType = "vfat";
  };

  swapDevices = [{ device = "/dev/disk/by-label/swap"; }];

  nix.settings.max-jobs = lib.mkDefault 12;
  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
  hardware.cpu.intel.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;
}
