let
  sources = import ./sources.nix;
  tooling = import sources.nix-tooling;
in
  tooling
