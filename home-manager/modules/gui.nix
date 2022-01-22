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

    kittyFontSize = lib.mkOption {
      type = lib.types.str;
      default = "10.0";
      description = "Kitty terminal font size.";
    };
  };

  config = (lib.mkIf cfg.enable {
    home.packages = [
      # Programming
      pkgs.vscode

      # Screenshot
      pkgs.scrot
      pkgs.xclip

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

      ".xmonad/xmonad.hs".source = ../../config/xmonad/xmonad.hs;
      ".xmonad/get-mic.sh".source = ../../config/xmonad/get-mic.sh;
      ".xmonad/get-volume.sh".source = ../../config/xmonad/get-volume.sh;

      ".xmobarrc".source = ../../config/xmobarrc;
      ".XCompose".source = ../../config/XCompose;
    };

    programs = {
      kitty = {
        enable = true;
        settings = {
          "scrollback_lines" = "10000";
          "repaint_delay" = "4";
          "font_family" = "Hasklug Nerd Font Complete Mono";
          "bold_font" = "Hasklug Bold Nerd Font Complete Mono";
          "italic_font" = "Hasklug Italic Nerd Font Complete Mono";
          "bold_italic_font" = "Hasklug Bold Italic Nerd Font Complete Mono";
          "disable_ligatures" = "never";
          "background_opacity" = "0.7";
          "dynamic_background_opacity" = "yes";
          "background_tint" = "0.8";
          "font_size" = cfg.kittyFontSize;
        };
      };

      mpv = {
        enable = true;
      };

      rofi = {
        enable = true;
        cycle = true;
        pass = { enable = true; };
        # theme = "docu";
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