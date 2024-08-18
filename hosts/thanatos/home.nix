{ dotfiles, pkgs, ... }:
{
  imports = with dotfiles.self.homeManagerModules; [
    fonts
    term
    dev.nix
    dev.tools
    editors.helix
    editors.neovim
  ];

  config = {
    packages = [ pkgs.openssh ];
  };
}
