/****************************************************************************
  * programs/dev/provers module
  *
  **************************************************************************/
{ pkgs, ... }:
{
  imports = [ ];

  config = {
    home.packages = [
      pkgs.agda
      pkgs.agdaPackages.standard-library
      pkgs.coq
      pkgs.idris2
      pkgs.lean4
    ];
  };
}
