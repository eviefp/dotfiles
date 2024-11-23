/****************************************************************************
  * dev/provers module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.dev.provers;
in
{
  options.evie.dev.provers = {
    enable = lib.mkEnableOption "provers defaults";
  };
  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.idris2
      pkgs.lean4
    ];
  };
}
