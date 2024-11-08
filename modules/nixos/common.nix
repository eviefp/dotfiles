/****************************************************************************
  * Common module
  *
  * This is basically just a shorthand for what most of my systems use.
  **************************************************************************/
{ dotfiles, ... }:
{
  imports = with dotfiles.self.nixosModules; [
    nix-settings
    boot
    network
    locale
    packages
    services
    users
    sops
    yubikey
  ];

  config = {

    nixpkgs.overlays = [ (import dotfiles.emacs-overlay) ];
    nixpkgs.config.allowUnfree = true;

    evie.packages = {
      enableGPG = true;
      enableDconf = true;
    };

    evie.services.xcompose = false;
  };
}
