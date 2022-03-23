# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [
      (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.kernelPackages = pkgs.linuxPackages_5_15;
  boot.initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "vmd" "nvme" "usb_storage" "sd_mod" "rtsx_usb_sdmmc" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  boot.kernelParams = [
    # https://bugzilla.redhat.com/show_bug.cgi?id=805285
    # fix for 'Queue <x> is active on fifo 1 and stuck for 10000 ms'
    "iwlwifi.wd_disable=1"
  ];

  # Try to fix iwlwifi issues (again)
  # https://bbs.archlinux.org/viewtopic.php?pid=1918191#p1918191
  boot.extraModprobeConfig = ''
    options iwlmvm power_scheme=1
    options iwlwifi 11n_disable=1
    options iwlwifi swcrypto=0
    options iwlwifi bt_coex_active=0
    options iwlwifi power_save=0
    options iwlwifi d0i3_disable=1
    options iwlwifi uapsd_disable=1
    options iwlwifi lar_disable=1
  '';


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
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
