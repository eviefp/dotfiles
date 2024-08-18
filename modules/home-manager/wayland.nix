{ dotfiles, lib, ... }:
{
  imports = with dotfiles.self.homeManagerModules; [
    wayland.hyprland.hyprland
    wayland.river

    wayland.swaync
    wayland.eww
    wayland.screenshot
    wayland.rofi
  ];

  options.evie.wayland = {
    eww-monitor = lib.mkOption {
      type = lib.types.str;
      default = "1";
    };

    showBattery = lib.options.mkEnableOption "Show battery widget?";
    showTV = lib.options.mkEnableOption "Show tv widget?";

    useSshMailCalendar = lib.options.mkEnableOption "Enable on non-thelxinoe";
    showMail = lib.options.mkEnableOption "Show email widget?";
    showCalendar = lib.options.mkEnableOption "Show calendar widget?";

    disabledMonitors = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
    };

    monitors = lib.mkOption {
      type = lib.types.listOf (lib.types.submodule {
        options = {
          name = lib.mkOption {
            type = lib.types.str;
            example = "DP-1";
          };
          resolution = lib.mkOption {
            type = lib.types.str;
            example = "1920x1080@239.76";
          };
          position = lib.mkOption {
            type = lib.types.str;
            example = "0x0";
          };
          keybind = lib.mkOption {
            type = lib.types.str;
            example = "W";
          };
        };
      });
      default = [ ];
    };
  };
}
