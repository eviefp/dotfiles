{ pkgs, config, lib, ... }:
let
  cfg = config.evie.wayland.waybar.wttrbar;
in
{
  options.evie.wayland.waybar.wttrbar = {
    enable = lib.mkEnableOption "wttrbar defaults";
  };

  config = lib.mkIf cfg.enable {

    home.file = {
      ".config/kitty/light.conf".text = ''
    };
  };
}

