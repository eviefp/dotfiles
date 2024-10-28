/****************************************************************************
  * Nix settings
  *
  **************************************************************************/
{ dotfiles, pkgs, lib, config, ... }:
{
  config.nix = {
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
        "flakes"
        "nix-command"
        "repl-flake"
      ];
      nix-path = lib.mkForce "nixpkgs=${dotfiles.nixpkgs}";
    };

    # package = pkgs.nixFlakes;
    nrBuildUsers = 50;
    extraOptions = ''
      binary-caches-parallel-connections = 5
      netrc-file = ${config.sops.secrets.garnix_netrc.path}
    '';
  };
}
