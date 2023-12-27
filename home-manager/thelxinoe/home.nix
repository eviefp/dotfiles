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
    ../modules/hyprland.nix
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

  home.sessionVariables = {
    EDITOR = "nvim";
    BROWSER = "qutebrowser";
    NIX_PATH = nix-path;
    OOO_FORCE_DESKTOP = "gnome";
  };
}
