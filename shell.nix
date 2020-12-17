let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs {
    config.allowUnfree = true;
  };
  home-manager = import sources.home-manager {};
in
  nixpkgs.mkShell {
    nativeBuildInputs = [];
    NIX_PATH = "nixpkgs=${nixpkgs.path}:home-manager=${home-manager.path}:nixos-config=/etc/nixos/configuration.nix";
  }
