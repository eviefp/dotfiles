{ lib, config, pkgs, ... }:
let
in
{
  imports = [ ];

  config = {
    home.packages = [
      # screenshot; pkgs.grimblast also woks with 'grimblast copy area'
      pkgs.grimblast
      # pkgs.grim
      # pkgs.slurp
      pkgs.wl-clipboard
    ];
  };
}
