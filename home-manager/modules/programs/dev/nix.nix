/****************************************************************************
  * programs/dev/nix module
  *
  * Enable packages I use for nix-related development:
  *   - 'niv' for pinning github repositories
  *   - 'nixfmt' for formatting sources with emacs/nvim (TODO)
  *   - 'nix-diff' for finding out how two derivations differ
  ************************************************************************ */
{ pkgs, nil, ... }:
{
  imports = [ ];

  config = {
    home.packages = [
      pkgs.haskellPackages.niv
      nil.packages.${pkgs.system}.nil
      pkgs.nix-diff
      pkgs.nixpkgs-fmt
    ];
  };
}
