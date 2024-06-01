/****************************************************************************
  * programs/dev/haskell module
  *
  **************************************************************************/
{ pkgs, ... }:
{
  imports = [ ];

  config = {
    home.packages = [
      pkgs.ghcid
      pkgs.haskellPackages.hp2html
      pkgs.haskellPackages.hp2pretty
    ];

    home.file = {
      ".ghci".source = ../../../../config/ghci;
    };

  };
}
