/****************************************************************************
  * dev/haskell module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.dev.haskell;
in
{
  options.evie.dev.haskell = {
    enable = lib.mkEnableOption "haskell defaults";
  };
  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.ghcid
      pkgs.ghciwatch
      pkgs.haskellPackages.hp2html
      pkgs.haskellPackages.hp2pretty
    ];

    home.file = {
      ".ghci".text = ''
        import Prelude

        :set prompt "λ "
      '';
    };

  };
}
