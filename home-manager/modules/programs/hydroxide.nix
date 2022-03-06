/****************************************************************************
  * Hydroxide module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.programs.hydroxide;
  package = pkgs.callPackage ./hydroxide-package.nix {};
in
{
  imports = [ ];

  options.evie.programs.hydroxide = {
    enable = lib.options.mkEnableOption "Enable hydroxide";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      package
    ];
  };
}
