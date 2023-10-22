{ config, lib, pkgs, ... }:
{
  modules = [
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
      };

      dev = {
        nix.enable = true;
        tools.enable = true;
        helix.enable = true;
        emacs = {
          enable = true;
          no-x = true;
        };
        neovim.enable = true;
      };

      ranger.enable = true;
    };
  };

  home.stateVersion = "23.11";

  home.programs.ssh.enable = true;

  home.sessionVariables = {
    EDITOR = "emacs";
    NIX_PATH = "nixpkgs=${nixpkgs}";
  };
}
