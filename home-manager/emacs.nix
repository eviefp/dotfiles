{ pkgs }:

let
  file = {
    ".emacs.d/init.el".source = ./init.el;
  };
  deriv =
    pkgs.emacsWithPackagesFromUsePackage {
      config = ./init.el;
      package = pkgs.emacsGit;
    };
in
  {
    derivation = deriv;
    file = file;
  }
