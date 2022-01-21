/****************************************************************************
  * dev/nix module
  *
  * Enable packages I use for nix-related development:
  *   - 'niv' for pinning github repositories
  *   - 'nixfmt' for formatting sources with emacs/nvim (TODO)
  *   - 'rnix-lsp' as the nix lsp
  *   - 'nix-diff' for finding out how two derivations differ
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.programs.dev.nix;
in
{
  imports = [ ];

  options.evie.programs.dev.nix = {
    enable = lib.options.mkEnableOption "Enable nix";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.haskellPackages.niv
      pkgs.nix-diff
      pkgs.nixfmt
      pkgs.rnix-lsp
    ];
  };
}
