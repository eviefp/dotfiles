{ dotfiles, ... }:
{
  imports = with dotfiles.self.homeModules; [
    common
  ];

  config.evie = {
    common.enable = true;
  };
}
