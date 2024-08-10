/****************************************************************************
  * GUI module
  *
  * GUI programs such as browsers, multimedia, etc.
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.programs.gui;
in
{
  imports = [
    ./programs/browsers.nix
    ./programs/kitty.nix
    # ./programs/wezterm.nix
    ./programs/chat.nix
  ];

  options.evie.programs.gui = {
    useLaptopXmobar = lib.options.mkEnableOption "Enable battery display.";
  };

  config = {
    home.packages = [
      # Multimedia
      pkgs.light
      pkgs.fdk_aac
      pkgs.paprefs # multi audio sink setup
      pkgs.pavucontrol
      pkgs.steam
      pkgs.transmission_4-gtk
      pkgs.xournal
      pkgs.libreoffice

      pkgs.gimp

      # X-server related
      pkgs.pass
      pkgs.passff-host
      pkgs.pinentry
      pkgs.xdg_utils

      pkgs.audacity

      pkgs.chromium
    ];

    home.file = {
      ".config/fish/functions/ssh.fish".source =
        ../../config/fish/functions/ssh.fish;

      ".config/fish/functions/ed.fish".source =
        ../../config/fish/functions/ed.fish;

      ".XCompose".source = ../../config/XCompose;
    };

    programs = {
      mpv = {
        enable = true;
      };
    };

    services = {
      stalonetray = {
        enable = false;
        package = pkgs.stalonetray;
        config = {
          decorations = "all";
          transparent = true;
          dockapp_mode = "none";
          geometry =
            if cfg.useLaptopXmobar
            then "6x1-210+1059"
            else "6x1-2120+1059";
          background = "#46224c";
          kludges = "force_icons_size";
          grow_gravity = "SW";
          icon_gravity = "SE";
          icon_size = 20;
          sticky = true;
          window_strut = "bottom";
          window_type = "dock";
          window_layer = "bottom";
          no_shrink = false;
          skip_taskbar = true;
          tint_level = 210;
          tint_color = "#46224c";
          parent_bg = true;
        };
      };

      xscreensaver = {
        enable = false;
        settings = {
          lock = false;
          mode = "blank";
          dpmsEnabled = true;
          splash = false;
          fade = false;
        };
      };
    };
  };
}
