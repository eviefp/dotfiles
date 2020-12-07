{ pkgs, locals }:

let
  file = {
    ".emacs.d/init.el".source = ./init.el;
    ".emacs.d/locals.el".source = locals;
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
