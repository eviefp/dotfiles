#*****************************************************************************
# dev/nix module
#
# Enable packages I use for nix-related development:
#   - 'niv' for pinning github repositories
#   - 'nixfmt' for formatting sources with emacs/nvim (TODO)
#   - 'nix-diff' for finding out how two derivations differ
#****************************************************************************
{ lib, config, pkgs, ... }:
let cfg = config.evie.dev.nix;
in {
  imports = [ ];

  options.evie.dev.nix = { enable = lib.options.mkEnableOption "Enable nix"; };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.haskellPackages.niv pkgs.nix-diff pkgs.nixfmt ];
  };
}
