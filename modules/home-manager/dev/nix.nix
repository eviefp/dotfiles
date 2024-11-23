/****************************************************************************
  * programs/dev/nix module
  *
  * Enable packages I use for nix-related development:
  *   - 'niv' for pinning github repositories
  *   - 'nixfmt' for formatting sources with emacs/nvim
  *   - 'nix-diff' for finding out how two derivations differ
  ************************************************************************ */
{ dotfiles, pkgs, ... }:
{
  config = {
    home.packages = [
      pkgs.haskellPackages.niv
      dotfiles.nil.packages.${pkgs.system}.nil
      pkgs.nix-diff
      pkgs.nixpkgs-fmt
    ];
  };
}
