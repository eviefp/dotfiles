/****************************************************************************
  * programs/dev/haskell module
  *
  **************************************************************************/
{ pkgs, ... }:
{
  config = {
    home.packages = [
      pkgs.ghcid
      pkgs.ghciwatch
      pkgs.haskellPackages.hp2html
      pkgs.haskellPackages.hp2pretty
    ];

    home.file = {
      ".ghci".source = ../../../../config/ghci;
    };

  };
}
