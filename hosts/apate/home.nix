{ dotfiles, ... }:
{
  imports = with dotfiles.self.homeModules; [
    common
  ];

  home = {
    stateVersion = "25.05";
    sessionVariables = {
      NIX_PATH = "nixpkgs=${dotfiles.nixpkgs}";
    };
  };

  config.evie = {
    common.enable = true;

    dev = {
      haskell.enable = true;
      lua.enable = true;
      provers.enable = true;
    };

    editors = {
      emacs.enable = false;
      neovim.obsidian = true;
    };
    system.gpg.enable = true;
    programs.firefox.enable = true;
    term.kitty.enable = true;

    # TODO: aerospace
  };
}
