let
  pkgs = import <nixpkgs> { };
  ep = [
    pkgs.xorg.libX11
    pkgs.xorg.libXft
    pkgs.xorg.libXrandr
    pkgs.xorg.libXdmcp
    pkgs.xorg.libXScrnSaver
    pkgs.xorg.libXext
    pkgs.expat
    pkgs.pkg-config
  ];
  shell = import ../../dev-shell/haskell/shell.nix { extraPackages = ep; };
in
shell
