{modulesPath, ...}: {
  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot.loader.grub = {
    enable = true;
    efiSupport = true;
    efiInstallAsRemovable = true;
  };

  boot.initrd = {
    availableKernelModules = ["ahci" "sd_mod" "thunderbolt" "xhci_pci" "usbhid" "nvme" "usb-storage"];
  };

  nixpkgs.hostPlatform = "x86_64-linux";

  system.stateVersion = "25.05";
}
