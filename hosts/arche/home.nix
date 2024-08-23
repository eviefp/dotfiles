{ dotfiles, pkgs, ... }:
{
  imports = with dotfiles.self.homeManagerModules; [
    common
    programs.streaming
    programs.dev.default
    gui
    wayland.default
  ];

  config = {

    home.packages = [
      pkgs.chromium
    ];

    evie = {
      system.user = "every";
      wayland = {
        eww-monitor = "0";
        showTV = true;
        useSshMailCalendar = false;
        showMail = true;
        showCalendar = true;
        monitors = [
          {
            name = "HDMI-4";
            resolution = "1920x1080@59.96";
            position = "0x0";
            keybind = "W";
          }
        ];
        disabledMonitors = [ "Unknown-1" ];
      };
    };
  };
}
