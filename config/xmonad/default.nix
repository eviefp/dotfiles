let
  sources = import ./nix/sources.nix;
  tooling = import sources.nix-tooling;
  pkgs = tooling.pkgs;
  gis = import sources.gitignore { inherit (pkgs) lib; };
in
  pkgs.haskell.packages.ghc8102.callCabal2nix "xmonad-project" (gis.gitignoreSource ./.) {}
