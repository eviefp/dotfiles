# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:
let
  lp = stdenv.mkDerivation rec {
    pname = "linux-kernel-patched-iwlwifi-5_16";
    version = "5.16.0";
    src = pkgs.linuxPackages_5_16;
    configurePhase = ''
      rm -rf lib/firmware/iwlwifi-ty-a0-gf-a0-66*
      rm -rf lib/firmware/iwlwifi-ty-a0-gf-a0-67*
      rm -rf lib/firmware/iwlwifi-ty-a0-gf-a0-68*
    '';
  };
in
{
  imports =
    [
      (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.kernelPackages = pkgs.linuxPackages_5_16;
  boot.initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "vmd" "nvme" "usb_storage" "sd_mod" "rtsx_usb_sdmmc" "iwlwifi" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

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
