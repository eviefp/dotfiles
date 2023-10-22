{ nixpkgs, ... }:
{
  imports = [
    ../modules/programs/dev/nix.nix
    ../modules/programs/dev/tools.nix
    ../modules/programs/editors/emacs.nix
    ../modules/programs/editors/helix.nix
    ../modules/programs/editors/neovim.nix
    ../modules/programs/shell.nix
    ../modules/programs/shell/ranger.nix
  ];

  evie = {
    programs = {
      shell = {
        enable = true;
        experimental = true;
        ranger.enable = true;
      };

      dev = {
        nix.enable = true;
        tools.enable = true;
      };
      editors = {
        helix.enable = true;
        emacs = {
          enable = true;
          no-x = true;
        };
        neovim.enable = true;
      };

    };
  };

  home.stateVersion = "23.11";

  home.programs.ssh.enable = true;

  home.sessionVariables = {
    EDITOR = "emacs";
    NIX_PATH = "nixpkgs=${nixpkgs}";
  };
}
