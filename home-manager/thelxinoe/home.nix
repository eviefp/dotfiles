/****************************************************************************
  * Thelxinoe home-manager
  *
  **************************************************************************/
{ nix-path, ... }:
{
  imports = [
    ../modules/programs.nix
    ../modules/gui.nix
    ../modules/programs/editors/emacs.nix
    ../modules/email.nix
    ../modules/programs/editors/neovim.nix
    ../modules/fonts.nix
    ../modules/system.nix
    ../modules/services/calendar-sync.nix
    ../modules/wayland.nix
  ];

  evie.programs = {
    enable = true;

    ect.enable = true;

    browsers.enable = true;
    bower.enable = true;
    chat.enable = true;
    kitty.enable = true;

    dev = {
      haskell.enable = true;
      lua.enable = true;
      nix.enable = true;
      provers.enable = true;
      tools.enable = true;
    };

    editors = {
      neovim.enable = true;

      emacs = {
        enable = true;
        service = true;
        no-x = false;
        locals = {
          enable = true;
          file = ./locals.el;
        };
      };

      vscode.enable = true;

      helix.enable = true;
    };

    shell = {
      enable = true;
      experimental = true;
      ranger.enable = true;
    };

    streaming.enable = true;

    text = {
      enable = true;
      latex = true;
    };

    gui.enable = true;

    wezterm.enable = true;
  };

  evie.email.enable = true;

  evie.fonts.enable = true;

  evie.system = {
    enable = true;
    host = "thelxinoe";
    dotfiles = "/home/evie/code/dotfiles";
  };

  evie.services = {
    calendar-sync.enable = true;
  };

  evie.wayland = {
    enable = true;
    eww-monitor = "2";
    showTV = true;
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
  };

  home.sessionVariables = {
    EDITOR = "nvim";
    BROWSER = "qutebrowser";
    NIX_PATH = nix-path;
    OOO_FORCE_DESKTOP = "gnome";
  };
}
