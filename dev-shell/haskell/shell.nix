{ pkgs ? import <nixpkgs> {}}:
let
  # Do not touch this part.
  def = import ./.;
in pkgs.mkShell {
  buildInputs = def.buildInputs;
  LD_LIBRARY_PATH = def.LD_LIBRARY_PATH;
}
