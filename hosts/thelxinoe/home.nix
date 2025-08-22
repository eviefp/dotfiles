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
      waybar = {
        enable = true;
        outputMonitor = "DP-2";
        modules = {
          enableTV = true;
          enableBT = true;
          enableEmails = true;
          enableCalendar = true;
        };
      };
      monitors = [
        {
          name = "DP-1";
          resolution = "1920x1080@239.76";
          position = "1920x395";
          keybind = "W";
        }
        {
          name = "DP-2";
          resolution = "1920x1080@239.76";
          position = "3840x395";
          keybind = "E";
        }
        {
          name = "DP-3";
          resolution = "1920x1080@239.76";
          position = "5760x0";
          transform = "3";
          keybind = "R";
        }
      ];
      disabledMonitors = [{
        name = "HDMI-A-1";
        keybind = "T";
      }];
    };
  };
}
