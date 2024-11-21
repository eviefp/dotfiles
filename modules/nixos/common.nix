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
    boot
    locale
    network
    nix-settings
    packages
    services
    sops
    users
    yubikey
  ];

  config = lib.mkIf cfg.enable {
    evie = {
      boot.enable = true;
      locale.enable = true;
    };

    nixpkgs.overlays = [ (import dotfiles.emacs-overlay) ];
    nixpkgs.config.allowUnfree = true;

    evie.packages = {
      enableDconf = true;
    };

    evie.services.xcompose = false;

    system.stateVersion = "25.05";
  };
}
