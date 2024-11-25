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
          "https://cache.lix.systems"
          "https://cache.garnix.io"
        ];
        extra-trusted-public-keys = [
          "cache.lix.systems:aBnZUw8zA7H35Cz2RyKFVs3H4PlGTLawyY5KRbvJR8o="
          "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
        ];
        extra-experimental-features = [
          "ca-derivations"
          "flakes"
          "nix-command"
          "no-url-literals"
          "repl-flake"
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
