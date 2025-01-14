{ dotfiles, ... }:
{
  imports = with dotfiles.self.homeManagerModules; [
    common
  ];

  config.evie = {
    common.enable = true;
    dev = {
      haskell.enable = true;
      lua.enable = true;
      provers.enable = true;
    };
    editors = {
      emacs = {
        enable = true;
        locals = {
          enable = true;
          file = ./aiode.el;
        };
      };
    };
    system = {
      gpg.enable = true;
    };

    wayland = {
      enable = true;
      hyprland.enable = true;
      eww = {
        enable = true;
        eww-monitor = "0";
        showBattery = true;
        useSshMailCalendar = true;
        showMail = false;
        showCalendar = false;
        showTV = true;
      };
      hypridle.lock = true;
      hyprpaper = {
        wallpaperSkip = 1;
      };
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
}
