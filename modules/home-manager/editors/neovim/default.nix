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
    ./telescope.nix
    ./web-devicons.nix

    # Movement
    ./arrow.nix
    ./leap.nix

    # Text editing / viewing
    ./blink-cmp.nix
    ./ccc.nix
    ./indent-o-matic.nix
    ./grug-far.nix
    ./gx.nix
    ./markview.nix
    ./origami.nix
    ./surround.nix
    ./tabular.nix
    ./time-machine.nix
    ./rainbow-delimiters.nix

    # Git related
    ./blame.nix
    ./co-author.nix
    ./gitsigns.nix
    ./git-conflict.nix
    ./neogit.nix

    # LSP
    ./action-preview.nix
    ./conform.nix
    ./fidget.nix
    ./lsp.nix
    ./lsp-lines.nix
    ./lsp-signature.nix

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
        # cursor effect
        smear-cursor = {
          enable = true;
        };

        # make w, e, and b respect words with camelCase, etc.
        spider = {
          enable = true;
          keymaps = {
            silent = true;
            motions = {
              w = "w";
              e = "e";
              b = "b";
            };
          };
        };

        # highlights FIX, TODO, HACK, WARN, PERF, NOTE, TEST. can be configured.
        todo-comments = {
          enable = true;
        };

        # enforce transparency
        transparent = {
          enable = true;
          settings = {
            extra_groups = [];
            exclude_grups = [];
          };
        };

        trouble = {
          enable = true;
          settings = {
            auto_refresh = true;
          };
        };

        which-key = {
          enable = true;
          settings = {
            preset = "classic";
            notify = true;
            plugins = {
              marks = true;
              registers = true;
              spelling = {
                enabled = true;
                suggestions = 20;
              };
              presets = {
                operators = true;
                motions = true;
                text_objects = true;
                windows = true;
                nav = true;
                z = true;
                g = true;
              };
            };
          };
        };

        # provides 'yow' to toggle wrapping
        wrapping = {
          enable = true;
        };

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
