{
  imports = [
    ../modules/nix-settings.nix
    ../modules/boot.nix
    ../modules/network.nix
    ../modules/locale.nix
    ../modules/packages.nix
    ../modules/services.nix
    ../modules/users.nix
  ];

  evie.packages = {
    enableGPG = true;
    enableDconf = true;
  };

  evie.services.xcompose = false;
}
