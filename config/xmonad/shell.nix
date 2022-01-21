let
  sources = import ../../dev-shell/haskell/nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  ep = [
    pkgs.xorg.libX11
    pkgs.xorg.libXft
    pkgs.xorg.libXrandr
    pkgs.xorg.libXScrnSaver
    pkgs.xorg.libXext
    pkgs.pkg-config
  ];
  shell = import ../../dev-shell/haskell/shell.nix { extraPackages = ep; };
in
shell
