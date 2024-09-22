{ dotfiles, ... }:
{
  imports = with dotfiles.self.homeManagerModules; [
    common
    sops
    programs.dev.default
    gui
    wayland.default
  ];

  config = {
    evie = {
      programs.editors.emacs.locals = {
        enable = true;
        file = ./aiode.el;
      };

      wayland = {
        wallpaperSkip = 1;
        eww-monitor = "0";
        showBattery = true;
        useSshMailCalendar = true;
        showMail = false;
        showCalendar = false;
        showTV = true;
        monitors = [
          {
            name = "eDP-1";
            resolution = "1920x1080";
            position = "0x0";
            keybind = "W";
          }
          {
            name = "DP-2";
            resolution = "1920x1080";
            position = "1920x0";
            keybind = "E";
          }
        ];
      };
    };
  };
}
