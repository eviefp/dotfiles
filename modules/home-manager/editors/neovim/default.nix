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
  theme,
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
    ./lualine.nix
    ./telescope.nix
    ./treesitter.nix

    # Text editing
    ./ccc.nix
    ./marks.nix
    ./leap.nix
    ./tabular.nix
    ./time-machine.nix

    # Git related
    ./blame.nix
    ./co-author.nix
    ./gitsigns.nix
    ./neogit.nix

    # Programming languages
    ./lsp.nix
    ./conform.nix

    ./lang/csharp.nix
    ./lang/haskell.nix
    ./lang/nix.nix
    ./lang/nushell.nix
    ./lang/purescript.nix
    ./lang/rust.nix
    ./lang/typescript.nix

    # Utilities / misc
    ./presenterm.nix
    ./sops.nix
  ];

  options.evie.editors.neovim = {
    enable = lib.mkEnableOption "neovim defaults";
    obsidian = lib.mkEnableOption "enable obsidian";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.obsidian

      # diagram
      pkgs.mermaid-cli
      pkgs.plantuml
      pkgs.d2
      pkgs.gnuplot

      # grug-far
      pkgs.ast-grep

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
          pattern = ["*.mdc"];
          command = ":setlocal filetype=markdown";
        }
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
        ## actions-preview
        {
          key = "<leader>ac";
          action = "<cmd>lua require('actions-preview').code_actions()<cr>";
          options.desc = "lsp: code actions";
        }
        # barbar
        {
          key = "gn";
          action = "<cmd>BufferNext<cr>";
          options.desc = "buffer: goto next";
        }
        {
          key = "gp";
          action = "<cmd>BufferPrevious<cr>";
          options.desc = "buffer: goto prev";
        }
        {
          key = "gN";
          action = "<cmd>BufferMoveNext<cr>";
          options.desc = "buffer: move next";
        }
        {
          key = "gP";
          action = "<cmd>BufferMovePrevious<cr>";
          options.desc = "buffer: move ";
        }
        {
          key = "<leader>bc";
          action = "<cmd>BufferClose<cr>";
          options.desc = "buffer: close";
        }
        {
          key = "<leader>bC";
          action = "<cmd>BufferCloseAllButCurrent<cr>";
          options.desc = "buffer: close all but current";
        }
        {
          key = "<leader>bp";
          action = "<cmd>BufferPin<cr>";
          options.desc = "buffer: pin";
        }
        # gx
        {
          key = "gx";
          action = "<cmd>Browse<cr>";
          options.desc = "browse link";
        }
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
        # lsp-lines
        {
          key = "<leader>dt";
          action = ":lua require('lsp_lines').toggle()<cr>";
          options.desc = "line diagnostics toggle";
        }
        # markview
        {
          key = "<leader>mv";
          action = "<cmd>Markview Toggle<cr>";
          options.desc = "markview toggle";
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
        ## UI
        web-devicons = {
          enable = true;
          settings = {
            default = true;
            variant = "dark";
          };
        };

        git-conflict = {
          enable = true;
        };

        #######################################################################
        # Experiments
        #######################################################################

        # TODO: sort through these
        indent-o-matic = {
          enable = true;
          settings = {
            skip_multiline = true;
            standard_widths = [2 4];
          };
        };

        rainbow-delimiters.enable = true;

        actions-preview = {
          enable = true;

          settings = {
            highlight_command.__raw =
              /*
              lua
              */
              ''
                {
                  require('actions-preview.highlight').delta 'delta --side-by-side',
                  require('actions-preview.highlight').diff_so_fancy(),
                  require('actions-preview.highlight').diff_highlight(),
                }
              '';
          };
        };

        # Arrow: cross-session jump through files.
        # + easy ui
        # - breaks folding
        arrow = {
          enable = true;
          settings = {
            show_icons = true;
            always_show_path = true;
            separate_by_branch = true;
            leader_key = ";";
          };
        };

        # barbar: tabline plugin
        barbar = {
          enable = true;
          settings = {
            highlight_visible = true;
          };
        };

        blink-cmp = {
          enable = true;
          settings = {
            keymap.preset = "default";
            appearance.nerd_font_variant = "mono";
            completion = {
              documentation.auto_show = true;
              menu.draw = {
                columns = [
                  {
                    __unkeyed = "kind_icon";
                  }
                  {
                    __unkeyed = "label";
                    gap = 1;
                  }
                ];
                components = {
                  label = {
                    text.__raw = ''
                      function(ctx)
                        return require("colorful-menu").blink_components_text(ctx)
                      end
                    '';
                    highlight.__raw = ''
                      function(ctx)
                        return require("colorful-menu").blink_components_highlight(ctx)
                      end
                    '';
                  };
                };
              };
            };
            sources.default = ["lsp" "path" "dictionary" "emoji" "latex-symbols"];
            fuzzy.implementation = "prefer_rust_with_warning";

            sources.providers = {
              path = {
                score_offset = 200;
              };
              lsp = {
                score_offest = 100;
              };
              latex-symbols = {
                module = "blink-cmp-latex";
                name = "Latex";
                score_offset = 50;
                opts = {
                  insert_command = false;
                };
              };
              emoji = {
                module = "blink-emoji";
                name = "emoji";
                score_offset = 30;
                opts = {
                  insert = false;
                };
              };
              dictionary = {
                module = "blink-cmp-dictionary";
                name = "Dict";
                score_offset = 1;
                min_keyword_length = 3;
              };
            };
          };
        };

        blink-emoji.enable = true;
        blink-cmp-latex.enable = true;
        blink-cmp-dictionary.enable = true;

        # Color completion menus.
        colorful-menu = {
          enable = true;
        };

        # Show mermaid diagrams in-line.
        diagram = {
          enable = true;
        };

        # Needed by 'diagram'.
        image = {
          enable = true;
          settings = {
            backend = "kitty";
          };
        };

        # Show LSP starting messages and other notifications.
        fidget = {
          enable = true;
          settings = {
            progress = {
              display.done_ttl = 10;
            };
            notification = {
              override_vim_notify = true;
              window = {
                border = "rounded";
              };
            };
            integration = {
              nvim-tree.enable = false;
            };
          };
        };

        # Find-and-Replace UI.
        # alternative: nvim-spectre
        grug-far = {
          enable = true;
          settings = {
            debounceMs = 1000;
            minSearchChars = 1;
            maxSearchMatches = 2000;
            normalModeSearch = false;
            maxWorkers = 8;
            engine = "ripgrep";
            engines = {
              ripgrep = {
                path = "rg";
                showReplaceDiff = true;
              };
            };
          };
        };

        # Open link in browser.
        # TODO: add handlers for stuff like "github:foo/bar" in flake.nix files or pkgs.bar, etc.
        # see https://github.com/chrishrb/gx.nvim
        gx = {
          enable = true;
        };

        # Indentation highlighter.
        hlchunk = {
          enable = true;
          settings = {
            chunk = {
              enable = true;
              use_treesitter = true;
            };
            indent = {
              enable = true;
              chars = ["│" "¦" "┆" "┊"];
            };
            line_num = {
              enable = true;
              style = "${theme.dark.purple}";
            };
            blank = {
              enable = true;
              chars = [" " "․" "⁚" "⁖" "⁘" "⁙"];
            };
          };
        };

        # lean lsp
        lean = {
          enable = true;
          settings = {
            mappings = true;
            lsp = {
              init_options = {
                edit_delay = 0;
                hasWidgets = true;
              };
            };

            abbreviations = {
              enable = true;
              extra = {
                # Add a \wknight abbreviation to insert ♘
                wknight = "♘";
              };
              leader = "\\";
            };

            infoview = {
              autoopen = false;
            };

            progress_bars = {
              enable = true;
              character = "|";
              priority = 10;
            };

            stderr = {
              enable = true;
              height = 5;
            };
          };
        };

        # In-line errors.
        lsp-lines = {
          enable = true;
        };

        # Show signature help for multi-param functions.
        lsp-signature = {
          enable = true;
          settings = {
            select_signature_key = "<C-n>";
          };
        };

        # Nicer markdown.
        markview = {
          enable = true;
          settings = {
            preview.map_gx = false;
            markdown = {
              enable = true;
              list_items = {
                enable = true; # try it again
              };
            };
          };
        };

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
