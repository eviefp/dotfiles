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
    ./telescope.nix
    ./web-devicons.nix

    # Movement
    ./arrow.nix
    ./leap.nix

    # Text editing / viewing
    ./blink-cmp.nix
    ./ccc.nix
    ./diagram.nix
    ./indent-o-matic.nix
    ./grug-far.nix
    ./gx.nix
    ./markview.nix
    ./presenterm.nix
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
    ./lang/nix.nix
    ./lang/nushell.nix
    ./lang/purescript.nix
    ./lang/rust.nix
    ./lang/typescript.nix

    # Utilities / misc
    ./sops.nix
  ];

  options.evie.editors.neovim = {
    enable = lib.mkEnableOption "neovim defaults";
    obsidian = lib.mkEnableOption "enable obsidian";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.obsidian

      # grug-far

      # snacks
      pkgs.ghostscript
    ];

    home.sessionVariables.EDITOR = "nvim";

    programs.nixvim = {
      enable = true;

      package = dotfiles.self.packages.${pkgs.system}.neovim;

      viAlias = true;
      vimAlias = true;

      autoCmd = [
        {
          event = ["BufEnter"];
          pattern = ["*.el"];
          command = ":setlocal filetype=lisp";
        }
      ];

      diagnostic.settings = {
        virtual_lines = {
          only_current_line = true;
        };
      };

      keymaps = [
        # Obsidian
        {
          key = "<leader>fn";
          action = ":Obsidian quick_switch<cr>";
          options.desc = "obsidian open";
        }
        {
          key = "<leader>ot";
          action = ":Obsidian tags<cr>";
          options.desc = "obsidian tags";
        }
        {
          key = "<leader>o1";
          action = ":Obsidian today<cr>";
          options.desc = "obsidian today";
        }
        {
          key = "<leader>o2";
          action = ":Obsidian tomorrow<cr>";
          options.desc = "obsidian tomorrow";
        }
        {
          key = "<leader>ol";
          action = ":Obsidian link<cr>";
          options.desc = "obsidian link";
        }
        {
          key = "<leader>oL";
          action = ":Obsidian link_new<cr>";
          options.desc = "obsidian link create";
        }
        {
          key = "<leader>or";
          action = ":Obsidian rename";
          options.desc = "obsidian rename";
        }
        {
          key = "<leader>ox";
          action = ":Obsidian extract_note<cr>";
          options.desc = "obsidian extract note";
        }
        # yazi
        {
          key = "<leader>ya";
          action = "<cmd>Yazi toggle<cr>";
          options.desc = "yazi toggle";
        }
        # origami
        {
          key = "<C-h>";
          action = ":lua require('origami').h()<cr>";
          options.desc = "fold region";
        }
        {
          key = "<C-l>";
          action = ":lua require('origami').l()<cr>";
          options.desc = "un-fold region";
        }
      ];

      plugins = {
        #######################################################################
        # Experiments
        #######################################################################
        # Search through nerd fonts.
        nerdy = {
          enable = true;
          enableTelescope = true;
        };

        # UI improvements
        noice = {
          enable = true;
          settings = {
            cmdline = {
              enabled = true;
              view = "cmdline_popup";
            };
            messages = {
              enabled = true;
              view_error = "notify";
              view_warn = "notify";
              view_history = "messages";
              view_search = "virtualtext";
            };
            popupmenu = {
              enabled = true;
              backend = "nui";
            };
            notify = {
              enabled = true;
              view = "notify";
            };
            lsp = {
              enabled = false;
            };
            presets = {
              bottom_search = true;
              command_palette = true;
              long_message_to_split = true;
              inc_rename = false;
              lsp_doc_border = true;
            };
          };
        };

        # Used by noice.
        nui.enable = true;

        # Used by noice.
        notify = {
          enable = true;
          settings = {
            level = "warn";
            background_colour = "#000000";
            max_width = 30;
          };
        };

        # surround; ys/yS
        nvim-surround = {
          enable = true;
        };

        obsidian = lib.mkIf cfg.obsidian {
          enable = true;
          settings = {
            legacy_commands = false;

            workspaces = [
              {
                name = "notes";
                path = "~/code/notes";
              }
            ];

            notes_subdir = "notes";

            daily_notes = {
              folder = "notes/daily";
              date_format = "%Y-%m-%d";
              alias_format = "%B %-d, %T";
              workdays_only = false;
            };

            completion = {
              blink = true;
              min_chars = 2;
            };

            new_notes_location = "notes_subdir";

            preferred_link_style = "wiki";

            picker = {
              name = "telescope.nvim";
            };

            ui = {
              enable = true;
              ignore_conceal_warn = true;
            };

            checkbox = {
              order = [" " ">" "x" "~"];
            };
          };
        };

        # edit files in a buffer
        oil = {
          enable = true;
          settings = {
            default_file_explorer = true;
            columns = ["icon" "permissions" "size" "mtime"];
            view_options = {
              show_hidden = true;
            };
          };
        };

        orgmode = {
          enable = true;
        };

        # folding
        origami = {
          enable = true;
          settings = {
            useLspFoldsWithTreesitterFallback = true;
            pauseFoldsOnSearch = true;
            foldtext = {
              enabled = true;
              padding = 2;
            };
            autoFold = {
              enabled = false;
            };
            foldKeymaps = {
              setup = false;
              hOnlyOpensOnFirstColumn = false;
            };
          };
        };

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
