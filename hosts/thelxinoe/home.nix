{ dotfiles, ... }:
{
  imports = with dotfiles.self.homeManagerModules; [
    common
    email
    sops
    programs.streaming
    programs.dev
    gui
    wayland
  ];

  config = {

    evie = {
      programs.editors.emacs.locals = {
        enable = true;
        file = ../../home-manager/locals/thelxinoe.el;
      };

      wayland = {
        eww-monitor = "0";
        showTV = true;
        useSshMailCalendar = false;
        showMail = true;
        showCalendar = true;
        monitors = [
          {
            name = "DP-1";
            resolution = "1920x1080@239.76";
            position = "0x0";
            keybind = "W";
          }
          {
            name = "DP-3";
            resolution = "1920x1080@239.76";
            position = "1920x0";
            keybind = "E";
          }
          {
            name = "DP-2";
            resolution = "1920x1080@239.76";
            position = "3840x0";
            keybind = "R";
          }
          {
            name = "HDMI-A-2";
            resolution = "1920x1080@60";
            position = "5760x0";
            keybind = "T";
          }
        ];
        disabledMonitors = [ "Unknown-1" ];
      };
    };
  };
}
