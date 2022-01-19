/*******************************************************************************
 * GUI module
 *
 * GUI programs such as browsers, multimedia, etc.
 ******************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.programs.gui;
in {
  imports = [];

  options.evie.programs.gui = {
    enable  = lib.options.mkEnableOption "Enable generic packages.";

    kittyFontSize = lib.mkOption {
      type = lib.types.str;
      default = "10.0";
      description = "Kitty terminal font size.";
    };
  };

  config = (lib.mkIf cfg.enable {
      home.packages = [
        # Browsers
        pkgs.chromium
        pkgs.firefox

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

        # X-server related
        pkgs.dmenu
        pkgs.haskellPackages.xmobar
        pkgs.pass
        pkgs.passff-host
        pkgs.pinentry_gnome
        pkgs.xdg_utils
      ];

      home.file = {
        ".config/tridactyl/tridactylrc".source = ../../config/tridactyl;
        ".mozilla/native-messaging-hosts/passff.json".source = "${pkgs.passff-host}/share/passff-host/passff.json";
        ".mozilla/native-messaging-hosts/tridactyl.json".source = "${pkgs.tridactyl-native}/lib/mozilla/native-messaging-hosts/tridactyl.json";

        ".config/fish/functions/clip.fish".source = ../../config/fish/functions/clip.fish;

        ".config/fish/functions/m0.fish".source = ../../config/fish/functions/m0.fish;
        ".config/fish/functions/m1.fish".source = ../../config/fish/functions/m1.fish;
        ".config/fish/functions/m2.fish".source = ../../config/fish/functions/m2.fish;
        ".config/fish/functions/rt.fish".source = ../../config/fish/functions/rt.fish;
        ".config/fish/functions/ssh.fish".source = ../../config/fish/functions/ssh.fish;
        ".config/fish/functions/ec.fish".source = ../../config/fish/functions/ec.fish;
        ".config/fish/functions/ed.fish".source = ../../config/fish/functions/ed.fish;

        ".xmonad/xmonad.hs".source = ../../config/xmonad/xmonad.hs;
        ".xmonad/get-mails.sh".source = ../../config/xmonad/get-mails.sh;
        ".xmonad/get-mic.sh".source = ../../config/xmonad/get-mic.sh;
        ".xmonad/get-volume.sh".source = ../../config/xmonad/get-volume.sh;

        ".xmobarrc".source = ../../config/carbon/xmobarrc;
        ".XCompose".source = ../../config/XCompose;
      };

      programs = {
        browserpass = {
          enable = true;
          browsers = [ "firefox" ];
        };


        mpv = {
          enable = true;
        };

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
            "background_opacity" = "0.9";
            "dynamic_background_opacity" = "yes";
            "background_tint" = "0.6";
            "background" = "#2b2b2b";
            "background_image" = "/home/evie/background.png";
            "font_size" = cfg.kittyFontSize;
          };
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
