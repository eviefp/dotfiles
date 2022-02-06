/****************************************************************************
  * Fractal home-manager
  *
  **************************************************************************/
{
  imports = [
    ../modules/programs.nix
    ../modules/programs/editors/emacs.nix
    ../modules/programs/editors/neovim.nix
    ../modules/fonts.nix
  ];

  evie.programs = {
    enable = true;

    dev = {
      haskell.enable = true;
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
      ranger.enable = true;
    };

    text = {
      enable = true;
      latex = true;
    };
  };

  evie.fonts.enable = true;

  evie.system = {
    enable = true;
    host = "fractal";
    dotfiles = " /home/evie/code/dotfiles ";
  };

  home.sessionVariables = {
    EDITOR = "nvim";
  };
}

