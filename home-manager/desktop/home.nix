let

  common = import ../common.nix;
in
{
  nixpkgs.config.nixpkgs.config.allowUnfree = true;

  home.packages =
    common.packages.generic
    ++ common.packages.nixos
    ++ common.packages.programming
    ++ common.packages.haskell
    ++ common.packages.provers
    ++ common.packages.scala
    ++ common.packages.latex
    ++ common.packages.streaming;

  home.sessionVariables = common.sessionVariables;

  programs = common.programs;

  services = common.services;
}
