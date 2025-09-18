{ dotfiles, ... }:
{
  imports = with dotfiles.self.homeModules; [
    editors.neovim
    system.home-manager
    term.term
    term.experimental
    term.scripts
    term.text
    term.tui
  ];
  config = {
    home.stateVersion = "25.05";

    evie = {
      editors.neovim.enable = true;
      system.home-manager.enable = true;
      term = {
        enable = true;
        experimental.enable = true;
        scripts.enable = true;
        text.enable = true;
        tui.enable = true;
      };
    };
  };
}
