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
  ];

  options.evie.programs.gui = {
    enable = lib.options.mkEnableOption "Enable generic UI packages.";
    useLaptopXmobar = lib.options.mkEnableOption "Enable battery display.";
  };

  config = (lib.mkIf cfg.enable {
    home.packages = [
      # Screenshot
      pkgs.scrot
      pkgs.xclip

      pkgs.shutter

      # Multimedia
      pkgs.fdk_aac
      pkgs.paprefs # multi audio sink setup
      pkgs.pavucontrol
      pkgs.steam
      pkgs.transmission-gtk
      pkgs.xournal

      pkgs.pipewire # used to create multi audio sinks

      # X-server related
      pkgs.haskellPackages.xmobar
      pkgs.pass
      pkgs.passff-host
      pkgs.pinentry-gnome
      pkgs.xdg_utils
    ];

    home.file = {

      ".config/fish/functions/clip.fish".source =
        ../../config/fish/functions/clip.fish;

      ".config/fish/functions/m0.fish".source =
        ../../config/fish/functions/m0.fish;
      ".config/fish/functions/rt.fish".source =
        ../../config/fish/functions/rt.fish;
      ".config/fish/functions/ssh.fish".source =
        ../../config/fish/functions/ssh.fish;
      ".config/fish/functions/ec.fish".source =
        ../../config/fish/functions/ec.fish;
      ".config/fish/functions/ed.fish".source =
        ../../config/fish/functions/ed.fish;
      ".config/fish/functions/tv.fish".source =
        ../../config/fish/functions/tv.fish;
      ".config/fish/functions/fixUI.fish".source =
        ../../config/fish/functions/fixUI.fish;

      ".xmonad/xmonad.hs".source = ../../config/xmonad/xmonad.hs;
      ".xmonad/get-mic.sh".source = ../../config/xmonad/get-mic.sh;
      ".xmonad/get-volume.sh".source = ../../config/xmonad/get-volume.sh;

      ".xmobarrc".source =
        if cfg.useLaptopXmobar
        then ../../config/xmobarrc-laptop
        else ../../config/xmobarrc;
      ".XCompose".source = ../../config/XCompose;
    };

    programs = {
      mpv = {
        enable = true;
      };

      rofi = {
        enable = true;
        cycle = true;
        pass = { enable = true; };
        theme = "Monokai";
      };
    };

    services = {
      stalonetray = {
        enable = true;
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
        enable = true;
        settings = {
          lock = false;
          mode = "blank";
          dpmsEnabled = true;
          splash = false;
          fade = false;
        };
      };
    };
  });
}
