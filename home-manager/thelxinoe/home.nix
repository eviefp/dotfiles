{
  imports = [
    ../modules/programs.nix
    ../modules/gui.nix
    ../modules/emacs.nix
    ../modules/email.nix
  ];

  evie.programs = {
    enable = true;
    haskell = true;
    provers = true;
    latex = true;
    streaming = true;

    gui.enable = true;

    emacs = {
      enable = true;
      locals = ../thelxinoe/locals.el;
    };
  };

  evie.email.enable = true;
}
