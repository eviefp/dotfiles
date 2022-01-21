/****************************************************************************
  * programs/dev/provers module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.programs.dev.provers;
in
{
  imports = [ ];

  options.evie.programs.dev.provers = {
    enable = lib.options.mkEnableOption "Enable provers";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.agda
      pkgs.agdaPackages.standard-library
      pkgs.coq
      pkgs.idris2
    ];
  };
}
