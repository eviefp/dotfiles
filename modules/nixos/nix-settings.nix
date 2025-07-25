/****************************************************************************
  * Nix settings
  *
  **************************************************************************/
{ dotfiles, lib, config, ... }:
let
  cfg = config.evie.nix-settings;
in
{
  options.evie.nix-settings = {
    enable = lib.options.mkEnableOption "Enable custom nix-settings.";
  };
  imports = [
    dotfiles.lix-module.nixosModules.default
  ];
  config = lib.mkIf cfg.enable {
    nix = {
      registry.nixpkgs.flake = dotfiles.nixpkgs;
      settings = {
        trusted-users = [ "root" "@wheel" "evie" ];
        extra-substituters = [
          "https://nix-community.cachix.org"
          "https://cache.lix.systems"
        ];
        extra-trusted-public-keys = [
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          "cache.lix.systems:aBnZUw8zA7H35Cz2RyKFVs3H4PlGTLawyY5KRbvJR8o="
        ];
        extra-experimental-features = [
          "flakes"
          "nix-command"
        ];
        nix-path = lib.mkForce "nixpkgs=${dotfiles.nixpkgs}";
      };

      nrBuildUsers = 50;

      extraOptions = ''
        binary-caches-parallel-connections = 5
        netrc-file = ${config.sops.secrets.garnix_netrc.path}
      '';
    };

    nixpkgs = {
      overlays = [ (import dotfiles.emacs-overlay) ];
      config.allowUnfree = true;
    };
  };
}
