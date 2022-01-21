/****************************************************************************
  * programs/dev/haskell module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.programs.dev.haskell;
in
{
  imports = [ ];

  options.evie.programs.dev.haskell = {
    enable = lib.options.mkEnableOption "Enable haskell";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.ghcid
      pkgs.haskellPackages.hp2html
      pkgs.haskellPackages.hp2pretty
      pkgs.stack
    ];

    home.file = {
      ".ghci".source = ../../../../config/ghci;
    };
  };
}
