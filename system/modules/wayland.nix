/****************************************************************************
  * Wayland module
  *
  **************************************************************************/
{ config, lib, pkgs, dotfiles, ... }:
let
  cfg = config.evie.wayland;
in
{
  options.evie.wayland = {
    compositor = lib.mkOption {
      type = lib.types.enum [ "hyprland" ];
      default = "hyprland";
      example = "hyprland";
      description = lib.mdDoc "Wayland compositor to use.";
    };

  };

  config = lib.mkIf (cfg.compositor == "wayland") {
    programs.hyprland = {
      enable = true;
      package = dotfiles.hyprland.packages.${pkgs.system}.hyprland;
      xwayland.enable = true;
    };

    services = {
      # TODO: try regreet
      displayManager.sddm = {
        enable = true;
        theme = "/run/current-system/sw/share/sddm/themes/elarun";
        wayland.enable = true;
      };
    };
  };
}
