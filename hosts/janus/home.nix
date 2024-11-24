{ dotfiles, ... }:
{
  imports = with dotfiles.self.homeManagerModules; [
    common
  ];

  config.evie = {

    common.enable = true;
    system = {
      gpg.enable = true;
    };

    wayland = {
      eww-monitor = "0";
      showBattery = true;
      useSshMailCalendar = false;
      showMail = false;
      showCalendar = false;
      disabledMonitors = [ "DP-1" ];
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
}
