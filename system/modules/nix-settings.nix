/****************************************************************************
  * Nix settings
  *
  **************************************************************************/
{ pkgs, ... }:
{
  imports = [ ];

  config = {

    nix = {
      settings = {
        trusted-users = [ "root" "evie" ];
        trusted-substituters = [
          "https://cache.lix.systems"
          "https://cache.garnix.io"
        ];

        trusted-public-keys = [
          "cache.lix.systems:aBnZUw8zA7H35Cz2RyKFVs3H4PlGTLawyY5KRbvJR8o="
          "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
        ];
      };

      package = pkgs.nixFlakes;
      nrBuildUsers = 50;
      extraOptions = ''
        experimental-features = nix-command flakes
        binary-caches-parallel-connections = 5
      '';
    };
  };
}
