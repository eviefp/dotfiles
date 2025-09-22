{lib, ...}: {
  options.evie.wayland = {
    enable = lib.mkEnableOption "wayland defaults";

    disabledMonitors = lib.mkOption {
      type = lib.types.listOf (lib.types.submodule {
        options = {
          name = lib.mkOption {
            type = lib.types.str;
            example = "DP-1";
          };
          keybind = lib.mkOption {
            type = lib.types.str;
            example = "W";
          };
        };
      });
      default = [];
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
          transform = lib.mkOption {
            type = lib.types.str;
            default = "0";
            example = "0";
          };
        };
      });
      default = [];
    };
  };
}
