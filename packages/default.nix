{ pkgs, pkgs-2411, ... }:
{
  isw = pkgs.callPackage ./isw.nix { };
  # peroxide uses buildGo122Module which has been deprecated in nixpkgs-25.05
  # TODO: use Hydroxide or fork peroxide
  peroxide = pkgs-2411.callPackage ./peroxide.nix { };

  waybar = pkgs.callPackage ./waybar.nix { systemdSupport = true; };
}
