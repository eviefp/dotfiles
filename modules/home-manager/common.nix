/****************************************************************************
  * Common home-manager module
  *
  * Adds the modules/settings that all of my systems use.
  **************************************************************************/
{ dotfiles, lib, config, ... }:
let
  cfg = config.evie.common;
  removeCommon = n: _: n != "common";
  notAttrs = n: !(lib.isAttrs n);
in
{
  options.evie.common = {
    enable = lib.mkEnableOption "common config";
  };

  imports = lib.collect notAttrs (lib.filterAttrs removeCommon dotfiles.self.homeModules);

  config = lib.mkIf cfg.enable
    (lib.mkMerge [
      {
        evie = {
          dev = {
            nix.enable = true;
            tools.enable = true;
          };
          editors = {
            neovim.enable = true;
            helix.enable = true;
          };

          system = {
            fonts.enable = true;
            home-manager.enable = true;
          };

          term = {
            enable = true;
            experimental.enable = true;
            scripts.enable = true;
            text.enable = true;
            tui.enable = true;
          };
        };
      }
      (lib.mkIf config.evie.wayland.enable {
        evie = {
          programs = {
            chat.enable = true;
            gui.enable = true;
            qutebrowser.enable = true;
            firefox.enable = true;
          };
          term = {
            kitty.enable = true;
          };

          wayland = lib.mkIf config.evie.wayland.hyprland.enable {
            rofi.enable = true;
            swaync.enable = true;
            waybar.enable = true;

            hypridle.enable = true;
            hyprlock.enable = true;
            hyprpaper.enable = true;
            hyprshade.enable = true;
          };
        };
      })
    ]
    );
}

