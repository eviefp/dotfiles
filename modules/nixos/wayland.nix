/****************************************************************************
  * Wayland module
  *
  **************************************************************************/
{ config, lib, dotfiles, pkgs, ... }:
let
  cfg = config.evie.wayland;
in
{
  options.evie.wayland = {
    enable = lib.mkEnableOption "wayland defaults";
    compositors = lib.mkOption {
      type = lib.types.listOf (lib.types.enum [ "hyprland" "river" "plasma" ]);
      default = [ "hyprland" ];
      example = [ "hyprland" "river" ];
      description = lib.mdDoc "Wayland compositor to use.";
    };

  };

  config = lib.mkIf cfg.enable (lib.mkMerge
    [
      {
        services = {
          displayManager.sddm = {
            enable = true;
            theme = "/run/current-system/sw/share/sddm/themes/elarun";
            wayland.enable = true;
          };
        };
      }

      (lib.mkIf (lib.elem "hyprland" cfg.compositors) {
        programs.hyprland = {
          enable = true;
          package = dotfiles.hyprland.packages.${pkgs.system}.hyprland;
          xwayland.enable = true;
        };
      })

      (lib.mkIf (lib.elem "river" cfg.compositors) {
        programs.river = {
          enable = true;
          xwayland.enable = true;
        };
      })

      (lib.mkIf (lib.elem "plasma" cfg.compositors) {
        services.desktopManager.plasma6.enable = true;
      })
    ]);
}
