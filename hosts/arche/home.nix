{ dotfiles, pkgs, ... }:
{
  imports = with dotfiles.self.homeManagerModules; [
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
        eww = {
          eww-monitor = "0";
          showTV = false;
          useSshMailCalendar = false;
          showMail = false;
          showCalendar = false;
        };
        monitors = [
          {
            name = "HDMI-A-%";
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
