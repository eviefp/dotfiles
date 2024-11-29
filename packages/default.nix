{ pkgs, ... }:
{
  isw = pkgs.callPackage ./isw.nix { };
  peroxide = pkgs.callPackage ./peroxide.nix { };
}
