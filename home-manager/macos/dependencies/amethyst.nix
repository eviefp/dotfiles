{ pkgs }:
let
  version = "0.15.3";

  release = pkgs.fetchzip {
    name = "Amethyst.app";
    url = "https://github.com/ianyh/Amethyst/releases/download/v${version}/Amethyst.zip";
    sha256 = "0li4ff4hhizf5ddipkyng5aps88z6j9kjbpl3zizh8inl2398xp9";
  };

  package = pkgs.runCommandCC "Amethyst-${version}" {} ''
    source $stdenv/setup
    mkdir -pv $out/Applications/Amethyst.app
    cp -r ${release}/* $out/Applications/Amethyst.app
  '';
in
  package


