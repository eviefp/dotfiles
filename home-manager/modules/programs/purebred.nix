/****************************************************************************
  * purebred module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  sources = import ../../../nix/sources.nix;
  purebred = import sources.purebred {};
  cfg = config.evie.programs.purebred;
in
{
  imports = [ ];

  options.evie.programs.purebred = {
    enable = lib.options.mkEnableOption "Enable purebred";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      purebred
    ];
  };
}
