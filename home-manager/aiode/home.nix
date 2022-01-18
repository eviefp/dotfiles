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

    gui = {
      enable = true;
      kittyFontSize = "12.0";
    };

    emacs = {
      enable = true;
      locals = ./locals.el;
    };
  };

  evie.email.enable = true;
}
