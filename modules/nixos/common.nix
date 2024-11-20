/****************************************************************************
  * Common module
  *
  * This is basically just a shorthand for what most of my systems use.
  **************************************************************************/
{ dotfiles, lib, config, ... }:
let
  cfg = config.evie.common;
in
{
  options.evie.common = {
    enable = lib.mkEnableOption "common config";
  };

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

  config = lib.mkIf cfg.enable {
    evie = {
      boot.enable = true;
    };

    nixpkgs.overlays = [ (import dotfiles.emacs-overlay) ];
    nixpkgs.config.allowUnfree = true;

    evie.packages = {
      enableDconf = true;
    };

    evie.services.xcompose = false;
  };
}
