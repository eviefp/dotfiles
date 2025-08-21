{ pkgs, pkgs-2411, ... }:
{
  isw = pkgs.callPackage ./isw.nix { };

  # peroxide uses buildGo122Module which has been deprecated in nixpkgs-25.05
  # TODO: use Hydroxide or fork peroxide
  peroxide = pkgs-2411.callPackage ./peroxide.nix { };

  waybar = pkgs.waybar.overrideAttrs (_: {
    src = pkgs.fetchFromGitHub {
      owner = "eviefp";
      repo = "Waybar";
      rev = "e8cf6a8f628cddb4c84e78ec0652e99178b43ec8";
      hash = "sha256-JlusLRHRIpb3YlFv5zR0MKdudvpvUqnXisEW1NGyGMI=";
    };
  });

  neovim = pkgs.neovim-unwrapped.overrideAttrs (_: {
    version = "0.12.0-dev";
    src = pkgs.fetchFromGitHub {
      owner = "neovim";
      repo = "neovim";
      rev = "c522cb0e96dad3bf3a834df5c6a8988f5c13a1a3";
      hash = "sha256-KPEN54yO3OQMxd+dn5lCw8dfvokfwzeTEqSEpy7e208=";
    };

  });
}
