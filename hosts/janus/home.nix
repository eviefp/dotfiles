{ dotfiles, pkgs, ... }:
{
  imports = with dotfiles.self.homeManagerModules; [
    common
  ];

  config = {
    home.packages = [
      pkgs.firefox
    ];

    evie = {
      common.enable = true;

      system = {
        gpg.enable = true;
      };

      wayland = {
        enable = true;
        hyprland.enable = true;
        eww = {
          eww-monitor = "0";
          showBattery = true;
          useSshMailCalendar = false;
          showMail = false;
          showCalendar = false;
        };
        disabledMonitors = [{
          name = "DP-1";
          keybind = "E";
        }];
        monitors = [
          {
            name = "eDP-1";
            resolution = "1920x1080@60.05";
            position = "0x0";
            keybind = "W";
          }
        ];
      };
    };
  };
}
