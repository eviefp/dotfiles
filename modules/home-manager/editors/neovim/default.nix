/**
**************************************************************************
* Neovim module
*
* Neovim package, plugins, and init file.
*************************************************************************
*/
{
  dotfiles,
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.evie.editors.neovim;
in {
  imports = [
    dotfiles.nixvim.homeModules.nixvim

    ./config.nix

    # Core
    ./colorscheme.nix
    ./keybindings.nix
    ./treesitter.nix

    # UI
    ./barbar.nix
    ./hlchunk.nix
    ./lualine.nix
    ./marks.nix
    ./noice.nix
    ./smear-cursor.nix
    ./telescope.nix
    ./transparent.nix
    ./trouble.nix
    ./web-devicons.nix
    ./which-key.nix

    # Movement
    ./arrow.nix
    ./leap.nix

    # Text editing / viewing
    ./blink-cmp.nix
    ./ccc.nix
    ./grug-far.nix
    ./gx.nix
    ./indent-o-matic.nix
    ./markview.nix
    ./origami.nix
    ./rainbow-delimiters.nix
    ./spider.nix
    ./surround.nix
    ./tabular.nix
    ./time-machine.nix
    ./todo-comments.nix
    ./wrapping.nix

    # Git related
    ./blame.nix
    ./co-author.nix
    ./git-conflict.nix
    ./gitsigns.nix
    ./neogit.nix

    # LSP
    ./action-preview.nix
    ./conform.nix
    ./fidget.nix
    ./lsp-lines.nix
    ./lsp-signature.nix
    ./lsp.nix

    # Programming languages
    ./lang/csharp.nix
    ./lang/haskell.nix
    ./lang/lean.nix
    ./lang/lisp.nix
    ./lang/nix.nix
    ./lang/nushell.nix
    ./lang/purescript.nix
    ./lang/rust.nix
    ./lang/typescript.nix

    # Utilities / misc
    ./diagram.nix
    ./obsidian.nix
    ./oil.nix
    ./presenterm.nix
    ./sops.nix
  ];

  options.evie.editors.neovim = {
    enable = lib.mkEnableOption "neovim defaults";
    obsidian = lib.mkEnableOption "enable obsidian";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      # snacks
      pkgs.ghostscript
    ];

    home.sessionVariables.EDITOR = "nvim";

    programs.nixvim = {
      enable = true;

      package = dotfiles.self.packages.${pkgs.system}.neovim;

      viAlias = true;
      vimAlias = true;

      keymaps = [
        # yazi
        {
          key = "<leader>ya";
          action = "<cmd>Yazi toggle<cr>";
          options.desc = "yazi toggle";
        }
      ];

      plugins = {
        #######################################################################
        # Experiments
        #######################################################################

        # yazi-inside-neovim
        yazi = {
          enable = true;
        };

        # visual multi cursors
        visual-multi = {
          enable = true;
          settings = {
            mouse_mappings = 0;
            silent_exit = 1;
            show_warnings = 1;
            default_mappings = 1;
          };
        };
      };
    };
  };
}
