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
      };

      wayland = {
        enable = true;
        eww-monitor = "0";
        showTV = false;
        useSshMailCalendar = false;
        showMail = false;
        showCalendar = false;
        monitors = [
          {
            name = "HDMI-4";
            resolution = "1920x1080@59.96";
            position = "0x0";
            keybind = "W";
          }
        ];
        disabledMonitors = [{
          name = "Unknown-1";
          keybind = "E";
        }];
      };
    };

    home.packages = [
      pkgs.chromium
    ];
  };
}
