{ dotfiles, pkgs, ... }:
{
  imports = with dotfiles.self.homeModules; [
    common
  ];

  config = {
    evie = {
      common.enable = true;

      system = {
        gpg.enable = true;
        home-manager = {
          user = "every";
        };
      };

      wayland = {
        enable = true;
        hyprland.enable = true;
        waybar = {
          enable = true;
          outputMonitor = "HDMI-A-4";
          modules = {
            enableBT = true;
          };
        };
        monitors = [
          {
            name = "HDMI-A-4";
            resolution = "1920x1080@60";
            position = "0x0";
            keybind = "W";
          }
        ];
      };
    };

    home.packages = [
      pkgs.chromium
    ];
  };
}
