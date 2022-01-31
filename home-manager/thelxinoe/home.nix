/****************************************************************************
  * Thelxinoe home-manager
  *
  **************************************************************************/
{
  imports = [
    ../modules/programs.nix
    ../modules/gui.nix
    ../modules/programs/editors/emacs.nix
    ../modules/email.nix
    ../modules/programs/editors/neovim.nix
    ../modules/fonts.nix
    ../modules/system.nix
  ];

  evie.programs = {
    enable = true;

    browsers.enable = true;

    chat.enable = true;

    dev = {
      haskell.enable = true;
      lua.enable = true;
      nix.enable = true;
      provers.enable = false;
      tools.enable = true;
    };

    editors = {
      neovim.enable = true;

      emacs = {
        enable = true;
        locals = {
          enable = true;
          file = ./locals.el;
        };
      };
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

  home.sessionVariables = {
    EDITOR = "nvim";
  };
}
