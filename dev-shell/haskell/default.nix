let
  # Do not touch this part.
  sources = import ../../nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  hls = import sources.haskell-language-server;

  deps = [ pkgs.zlib ];

  # Common tooling used by the whole team/CI. Remove if you'd rather
  # use the system-wide version you might already have.
  commonTooling = [
    pkgs.haskell.compiler.ghc8107 # GHC version as defined above at 'self'
    pkgs.haskell.packages.ghc8107.cabal-install
  ];

  hlsPackage = hls.outputs.packages.x86_64-linux.haskell-language-server;
  dynamicHls = hlsPackage.overrideAttrs(attrs: {
    # configureFlags = ["--enable-executable-dynamic"];
  });

  # Personal tooling: change/update as needed, e.g. `inputs.ghcide`.
  personalTooling = [
    dynamicHls
    pkgs.haskell.packages.ghc8107.cabal2nix
    pkgs.haskell.packages.ghc8107.implicit-hie
    pkgs.haskell.packages.ghc8107.hoogle
    pkgs.ghcid
  ];
in
{
  buildInputs = deps ++ commonTooling ++ personalTooling;
  LD_LIBRARY_PATH = "${pkgs.zlib}/lib";
}
