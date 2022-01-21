/****************************************************************************
  * Thelxinoe home-manager
  *
  **************************************************************************/
{
  imports = [
    ../modules/programs.nix
    ../modules/gui.nix
    ../modules/emacs.nix
    ../modules/email.nix
    ../modules/nvim.nix
  ];

  evie.programs = {
    enable = true;
    dev = {
      nix.enable = true;
      tools.enable = true;
    };
    text = {
      enable = true;
      latex = true;
    };
    shell = {
      enable = true;
      ranger.enable = true;
    };

    haskell = true;
    provers = true;
    streaming = true;

    gui.enable = true;

    nvim.enable = true;

    emacs = {
      enable = true;
      locals = {
        enable = true;
        file = ./locals.el;
      };
    };
  };

  evie.email.enable = true;
}
