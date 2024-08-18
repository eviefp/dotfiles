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
        file = ./janus.el;
      };

      wayland = {
        eww-monitor = "0";
        showBattery = true;
        useSshMailCalendar = true;
        showMail = true;
        showCalendar = true;
        monitors = [
          {
            name = "DP-1";
            resolution = "1920x515@60.075001";
            position = "0x1080";
            keybind = "E";
          }
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
