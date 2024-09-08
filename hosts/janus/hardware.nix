/****************************************************************************
  * Janus hardware configuration
  *
  **************************************************************************/
{ dotfiles, config, lib, pkgs, ... }:
let
  asus-wmi-screenpad = dotfiles.asus-wmi-screenpad.defaultPackage.x86_64-linux.override kernelPackages.kernel;
  kernelPackages = config.boot.kernelPackages;
in
{
  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "thunderbolt"
    "vmd"
    "nvme"
    "usb_storage"
    "sd_mod"
    "rtsx_usb_sdmmc"
  ];
  boot.initrd.kernelModules = [ "i915" ];
  boot.kernelModules = [ "kvm-intel" "ddcci" "asus-wmi-screenpad" ];
  boot.blacklistedKernelModules = [ "nouveau" ];
  boot.extraModulePackages = with kernelPackages; [ ddcci-driver turbostat asus-wmi-screenpad ];

  hardware.enableRedistributableFirmware = true;
  hardware.cpu.intel.updateMicrocode = true;

  fileSystems."/" =
    {
      device = "/dev/disk/by-label/nixos";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-label/boot";
      fsType = "vfat";
    };

  swapDevices = [{ device = "/dev/disk/by-label/swap"; }];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  console.font =
    lib.mkDefault "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";

  nixpkgs.hostPlatform = "x86_64-linux";
}
