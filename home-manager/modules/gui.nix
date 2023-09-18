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
      # Programming
      pkgs.vscode

      # Screenshot
      pkgs.scrot
      pkgs.xclip

      pkgs.shutter

      # Multimedia
      pkgs.fdk_aac
      pkgs.feh
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
      pkgs.pinentry_gnome
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
        enable = false;
        package = pkgs.stalonetray;
        config = {
          decorations = "all";
          transparent = false;
          dockapp_mode = "none";
          geometry = "6x1-0+0";
          background = "#000000";
          kludges = "force_icons_size";
          grow_gravity = "NW";
          icon_gravity = "NE";
          icon_size = 20;
          sticky = true;
          window_strut = "auto";
          window_type = "dock";
          window_layer = "bottom";
          no_shrink = false;
          skip_taskbar = true;
        };
      };
    };
  });
}
