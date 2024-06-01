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
        extra-substituters = [
          "https://cache.lix.systems"
        ];

        trusted-public-keys = [
          "cache.lix.systems:aBnZUw8zA7H35Cz2RyKFVs3H4PlGTLawyY5KRbvJR8o="
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
