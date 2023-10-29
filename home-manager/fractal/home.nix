/****************************************************************************
  * Fractal home-manager
  *
  **************************************************************************/
{ nix-path, ... }:
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
        enable = true;
        no-x = true;
        service = true;
        locals = {
          enable = true;
          file = ./locals.el;
        };
      };

      helix.enable = true;
    };

    shell = {
      enable = true;
      experimental = true;
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
    NIX_PATH = nix-path;
  };
}

