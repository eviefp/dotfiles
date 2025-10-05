{
  pkgs,
  pkgs-2411,
  lib,
  ...
}:
{
  isw = pkgs.callPackage ./isw.nix {};

  # peroxide uses buildGo122Module which has been deprecated in nixpkgs-25.05
  # TODO: use Hydroxide or fork peroxide
  peroxide = pkgs-2411.callPackage ./peroxide.nix {};

  # TODO: try patching instead
  waybar = pkgs.waybar.overrideAttrs (_: {
    src = pkgs.fetchFromGitHub {
      owner = "eviefp";
      repo = "Waybar";
      rev = "1b2d552c02cd5337fa47a924a85760538bc0656b";
      hash = "sha256-CwwJnnERYR1CUVLywGsjqiyBLv8qDs1ss9g8syiOANY=";
    };
  });

  neovim = pkgs.neovim-unwrapped.overrideAttrs (_: {
    version = "0.12.0-dev";
    src = pkgs.fetchFromGitHub {
      owner = "neovim";
      repo = "neovim";
      rev = "ea124068f246eeb5e436c1fc62c9379d9b936f15";
      hash = "sha256-4goAzSWn2el9o2r3OB4ywOiCmZ+3k4+MB+Q9M1vuN0I=";
    };
  });
}
// (import ./scripts.nix {inherit pkgs lib;})
