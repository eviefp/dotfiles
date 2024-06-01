{ pkgs ? import <nixpkgs> { }, extraPackages ? [ ] }:
let
  # Do not touch this part.
  def = import ./.;
in
pkgs.mkShell {
  buildInputs = def.buildInputs ++
    [
      pkgs.zlib.dev
    ] ++ extraPackages;
  #LD_LIBRARY_PATH = def.LD_LIBRARY_PATH;
}
