{ lib, config, pkgs, ... }:
let
  cfg = config.evie.wayland;
in
{
  imports = [ ];

  config = lib.mkIf cfg.enable {
    home.packages = [
      # screenshot; pkgs.grimblast also woks with 'grimblast copy area'
      pkgs.grimblast
      # pkgs.grim
      # pkgs.slurp
      pkgs.wl-clipboard
    ];
  };
}
