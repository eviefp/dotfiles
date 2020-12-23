let
  tooling = import ./nix/default.nix;
  pkgs = tooling.pkgs;
  self = tooling.haskell.ghc8102;
in
  pkgs.mkShell {
    buildInputs = self.defaultInputs ++
      [ pkgs.xorg.libX11 pkgs.xorg.libXrandr pkgs.xorg.libXext pkgs.xorg.libXScrnSaver pkgs.pkg-config pkgs.xorg.libXft
      ];
    }
