/****************************************************************************
  * hcs module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  sources = import ../../../nix/sources.nix;
  hcs = import sources.hcs { pkgs = pkgs; };
  cfg = config.evie.programs.hcs;
in
{
  imports = [ ];

  options.evie.programs.hcs = {
    enable = lib.options.mkEnableOption "Enable hcs";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      hcs
    ];
  };
}
