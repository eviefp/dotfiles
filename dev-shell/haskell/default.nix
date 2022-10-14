let
  # Do not touch this part.
  sources = import ../../nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  deps = [ pkgs.zlib ];

  # Common tooling used by the whole team/CI. Remove if you'd rather
  # use the system-wide version you might already have.
  commonTooling = [
    pkgs.haskell.compiler.ghc924 # GHC version as defined above at 'self'
    pkgs.haskell.packages.ghc924.cabal-install
  ];

  # Personal tooling: change/update as needed, e.g. `inputs.ghcide`.
  personalTooling = [
    # dynamicHls
    pkgs.haskell.packages.ghc924.cabal2nix
    pkgs.haskell.packages.ghc924.implicit-hie
    pkgs.haskell.packages.ghc924.hoogle
    pkgs.haskell.packages.ghc924.json-to-haskell
    pkgs.haskell.packages.ghc924.haskell-language-server
  ];
in
{
  buildInputs = deps ++ commonTooling ++ personalTooling;
  pkgs = pkgs;
}
