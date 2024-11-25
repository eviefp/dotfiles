# TODO: export this as a script and use that in hyprland config
{ config, lib, pkgs, ... }:
let
  cfg = config.evie.wayland.screenshot;
in
{
  options.evie.wayland.screenshot = {
    enable = lib.mkEnableOption "screenshot utils";
  };
  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.grimblast
      pkgs.wl-clipboard
    ];
  };
}
