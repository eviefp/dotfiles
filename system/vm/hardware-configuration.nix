# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, ... }:

{
  imports = [ ];

  boot.initrd.availableKernelModules = [ "ohci_pci" "ahci" "sd_mod" "sr_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];
  boot.initrd.checkJournalingFS = false;

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/2bfb9741-a660-4cd2-b8a1-dc9bf7704737";
      fsType = "ext4";
    };
  fileSystems."/virtualboxshare" = {
    fsType = "vboxsf";
    device = "shared";
    options = ["rw"];
  };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/21ef68f4-eae0-404c-9fc4-a8ca4f3fb69b"; }
    ];

  nix.maxJobs = lib.mkDefault 8;
  virtualisation.virtualbox.guest.enable = true;
}