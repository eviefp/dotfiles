/****************************************************************************
  * Janus home-manager
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
    ../modules/wayland.nix
  ];

  evie.programs = {
    enable = true;


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

    gui = {
      enable = true;
    };

    wezterm.enable = true;
  };

  evie.email.enable = false;

  evie.fonts.enable = true;

  evie.system = {
    enable = true;
    host = "janus";
    dotfiles = "/home/evie/code/dotfiles";
  };

  evie.wayland = {
    enable = true;
    eww-monitor = "1";
    monitors = [
      {
        name = "eDP-1";
        resolution = "1920x1080@60.05";
        position = "0x0";
        keybind = "W";
      }
      {
        name = "DP-3";
        resolution = "1920x515@60.07";
        position = "0x1080";
        keybind = "E";
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
