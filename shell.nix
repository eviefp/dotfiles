let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs {
    config.allowUnfree = true;
  };
  home-manager = import sources.home-manager {};
in
  nixpkgs.mkShell {
    nativeBuildInputs = [];
    NIX_PATH = "nixpkgs=${sources.nixpkgs}:home-manager=${sources.home-manager}:nixos-config=/etc/nixos/configuration.nix";
  }
