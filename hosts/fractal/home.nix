{ dotfiles, ... }:
{
  imports = with dotfiles.self.homeManagerModules; [
    common
    programs.dev.default
  ];

  config = {
    evie = {
      programs.editors.emacs.locals = {
        enable = true;
        file = ./fractal.el;
      };
    };
  };
}
