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
    ../modules/system.nix
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
        enable = false;
        locals = {
          enable = true;
          file = ./locals.el;
        };
      };
    };

    neuron = {
      enable = true;
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
    dotfiles = "/home/evie/code/dotfiles";
  };

  home.sessionVariables = {
    EDITOR = "nvim";
  };
}

