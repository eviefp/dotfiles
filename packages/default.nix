{ pkgs, ... }:
{
  peroxide = pkgs.callPackage ./peroxide.nix { };
  nuShellScript = pkgs.callPackage ./nuShellScript.nix { };
}
