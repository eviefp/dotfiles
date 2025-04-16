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
          file = ./thelxinoe.el;
        };
      };
      neovim.obsidian = true;
    };

    programs = {
      streaming.enable = true;
      twitch-tui.enable = true;
    };

    system = {
      calendar.enable = true;
      email.enable = true;
      gpg.enable = true;
    };

    wayland = {
      enable = true;
      hyprland.enable = true;
      eww = {
        eww-monitor = "2";
        showTV = true;
        useSshMailCalendar = false;
        showMail = true;
        showCalendar = true;
      };
      monitors = [
        {
          name = "DP-3";
          resolution = "1920x1080@239.76";
          position = "0x0";
          keybind = "W";
        }
        {
          name = "DP-1";
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
          name = "HDMI-A-1";
          resolution = "1920x1080@60";
          position = "5760x0";
          keybind = "T";
        }
      ];
    };
  };
}
