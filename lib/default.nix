{ pkgs }:
{
  nuShellScript = pkgs.callPackage ./nuShellScript.nix { };
  theme = import ./theme.nix;
}
