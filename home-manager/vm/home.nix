let
  common = import ../common.nix;
  emacs = import ../emacs.nix {
    pkgs = common.nixpkgs;
    locals = ./locals.el;
  };
in
{
  nixpkgs.config.nixpkgs.config.allowUnfree = true;

  accounts = common.accounts;

  home.packages =
    common.packages.generic
    ++ [ emacs.derivation ]
    ++ common.packages.nixos
    ++ common.packages.programming
    ++ common.packages.haskell
    ++ common.packages.provers
    ++ common.packages.scala
    ++ common.packages.latex
    ++ common.packages.streaming;

  home.sessionVariables = common.sessionVariables;
  home.file = common.file // emacs.file;

  programs = common.programs;

  services = common.services;
}
