{ dotfiles, ... }:
{
  imports = with dotfiles.self.homeModules; [
    common
  ];
  config = {
    home.stateVersion = "25.05";

    evie = {
      common.enable = true;
    };
  };
}
