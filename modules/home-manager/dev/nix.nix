/**
**************************************************************************
* dev/nix module
*
* Enable packages I use for nix-related development:
*   - 'niv' for pinning github repositories
*   - 'nixpkgs-fmt' for formatting sources with emacs/nvim
*   - 'nix-diff' for finding out how two derivations differ
************************************************************************
*/
{
  dotfiles,
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.evie.dev.nix;
in {
  options.evie.dev.nix = {
    enable = lib.mkEnableOption "nix defaults";
  };
  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.haskellPackages.niv
      dotfiles.nil.packages.${pkgs.system}.nil
      pkgs.nix-diff
      pkgs.nixpkgs-fmt
    ];
  };
}
