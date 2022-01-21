{ extraPackages ? [ ] }:
let
  # Do not touch this part.
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  deps = [ pkgs.zlib ];

  # Common tooling used by the whole team/CI. Remove if you'd rather
  # use the system-wide version you might already have.
  commonTooling = [
    pkgs.haskell.compiler.ghc8107 # GHC version as defined above at 'self'
    pkgs.haskell.packages.ghc8107.cabal-install
  ];

  # Personal tooling: change/update as needed, e.g. `inputs.ghcide`.
  personalTooling = [
    pkgs.haskell.packages.ghc8107.haskell-language-server
    pkgs.haskell.packages.ghc8107.cabal2nix
    pkgs.haskell.packages.ghc8107.implicit-hie
    pkgs.ghcid
  ];
in
pkgs.mkShell {
  buildInputs = deps ++ commonTooling ++ personalTooling ++ extraPackages;

  LD_LIBRARY_PATH = "${pkgs.zlib}/lib";
}
