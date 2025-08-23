/****************************************************************************
  * Neovim module
  *
  * Neovim package, plugins, and init file.
  **************************************************************************/
{ dotfiles, config, lib, pkgs, ... }:
let
  cfg = config.evie.editors.neovim;
in
{
  imports = [
    dotfiles.nixvim.homeModules.nixvim
  ];

  options.evie.editors.neovim = {
    enable = lib.mkEnableOption "neovim defaults";
    obsidian = lib.mkEnableOption "enable obsidian";

  };

  config = lib.mkIf cfg.enable {

    home.packages = [
      pkgs.obsidian
      pkgs.shellcheck
      pkgs.shfmt
      pkgs.haskellPackages.cabal-fmt
    ];

    home.sessionVariables.EDITOR = "nvim";

    programs.nixvim = {
      enable = true;

      package = dotfiles.self.packages.${pkgs.system}.neovim;

      viAlias = true;
      vimAlias = true;

      autoCmd = [
        {
          event = [ "BufEnter" ];
          pattern = [ "*.purs" ];
          command = ":setlocal filetype=purescript";
        }
      ];

      globals = {
        mapleader = " ";
        maplocalleader = ",";
        markdown_recommended_style = 0;
      };

      opts = {
        number = true;
        relativenumber = true;

        splitright = true;
        splitbelow = true;

        shiftwidth = 2;
        shiftround = true;
        expandtab = true;

        termguicolors = true;

        autoread = true;
        backup = false;
        swapfile = false;

        visualbell = false;
        errorbells = false;

        clipboard = "unnamedplus";

        showtabline = 2;

        fillchars = "eob:☭";

        conceallevel = 1;
      };

      keymaps = [
        {
          key = "\\";
          action = ":nohl<cr>";
        }
        {
          key = "<up>";
          action = "";
        }
        {
          key = "<down>";
          action = "";
        }
        {
          key = "<left>";
          action = "";
        }
        {
          key = "<right>";
          action = "";
        }
        {
          key = "<leader>fx";
          action = ":let @+ = expand(\"%\")<cr>";
        }
        {
          key = "<leader>wv";
          action = ":vsplit<cr>";
        }
        {
          key = "<leader>ws";
          action = ":split<cr>";
        }
        {
          key = "<leader>wq";
          action = ":q<cr>";
        }
        {
          key = "<leader>ww";
          action = ":w<cr>";
        }
        {
          key = "<leader>tt";
          action = ":vsplit<cr>:term<cr>a";
        }
        {
          key = "<leader>ts";
          action = ":split<cr>:term<cr>a";
        }
        {
          key = "<esc>";
          action = "<C-\\><C-n>";
          mode = "t";
        }
        ## lsp
        {
          key = "gD";
          action = ":vsplit<cr>:lua vim.lsp.buf.definition()<cr>";
        }
        {
          key = "<leader>ac";
          action = "<cmd>lua require('actions-preview').code_actions()<cr>";
        }
        {
          key = "<C-t>";
          action = "<cmd>lua require('FTerm').toggle()<cr>";
        }
        {
          key = "<leader>gb";
          action = "<cmd>BlameToggle<cr>";
        }
        ## Git/gitsigns
        {
          key = "<leader>gs";
          action = "<cmd>Neogit cwd=%:p:h<cr>";
        }
        {
          key = "]g";
          action = "<cmd>Gitsigns next_hunk<cr>";
        }
        {
          key = "]G";
          action = "<cmd>Gitsigns prev_hunk<cr>";
        }
        {
          key = "[g";
          action = "<cmd>Gitsigns prev_hunk<cr>";
        }
        {
          key = "[G";
          action = "<cmd>Gitsigns next_hunk<cr>";
        }
        ## Bufferline
        {
          key = "gn";
          action = "<cmd>BufferLineCycleNext<cr>";
        }
        {
          key = "gp";
          action = "<cmd>BufferLineCyclePrev<cr>";
        }
        {
          key = "<leader>bc";
          action = ":lua require('bufdelete').bufdelete()<cr>";
        }
        {
          key = "<leader>bC";
          action = "<cmd>BufferLineCloseOthers<cr>";
        }
        ## ccc
        {
          key = "<leader>cc";
          action = "<cmd>CccPick<cr>";
        }
        # sops
        {
          key = "<leader>ce";
          action = "<cmd>SopsEncrypt<cr>";
        }
        {
          key = "<leader>cd";
          action = "<cmd>SopsDecrypt<cr>";
        }
        #
        {
          key = "<leader>ta";
          action = ":Tabularize ";
        }
        # Obsidian
        {
          key = "<leader>fn";
          action = ":Obsidian quick_switch<cr>";
        }
        {
          key = "<leader>ot";
          action = ":Obsidian tags<cr>";
        }
        {
          key = "<leader>o1";
          action = ":Obsidian today<cr>";
        }
        {
          key = "<leader>o2";
          action = ":Obsidian tomorrow<cr>";
        }
        {
          key = "<leader>ol";
          action = ":Obsidian link<cr>";
        }
        {
          key = "<leader>oL";
          action = ":Obsidian link_new<cr>";
        }
        {
          key = "<leader>or";
          action = ":Obsidian rename";
        }
        {
          key = "<leader>ox";
          action = ":Obsidian extract_note<cr>";
        }
      ];

      extraPlugins = [
        pkgs.vimPlugins.nvim-nu
        pkgs.vimPlugins.nvim-surround
        pkgs.vimPlugins.lualine-lsp-progress
        pkgs.vimPlugins.tiny-inline-diagnostic-nvim # nicer diagnostic, overlaps with ]d a bit
        (pkgs.vimUtils.buildVimPlugin {
          pname = "blame.nvim";
          version = "v1.0";
          src = pkgs.fetchFromGitHub {
            owner = "FabijanZulj";
            repo = "blame.nvim";
            rev = "b87b8c820e4cec06fbbd2f946b7b35c45906ee0c";
            hash = "sha256-v4ieZ7NIWP1khvrcyzTSGX6IHHn0kjZICbyRqS2xqHM=";
          };
        }) # :Blame, but would be nice if it could go recursively
        pkgs.vimPlugins.outline-nvim # lsp outline, todo keybind
        pkgs.vimPlugins.mkdir-nvim # auto-mkdir when dir does not exist when saving files
        pkgs.vimPlugins.FTerm-nvim
        (pkgs.vimUtils.buildVimPlugin {
          pname = "co-author.nvim";
          version = "v1.0";
          src = pkgs.fetchFromGitHub {
            owner = "2KAbhishek";
            repo = "co-author.nvim";
            rev = "2f012714247dfe1959ba53fa50e4b1320d86d1b8";
            hash = "sha256-5/UORMt9TxOM7LRDKSbRymBt11XPe3OWN/8rwy0IkZg=";
          };
        }) # :CoAuthor
        pkgs.vimPlugins.nvim-sops
        pkgs.vimPlugins.tabular # :Tabularize
        pkgs.vimPlugins.actions-preview-nvim
      ];

      extraConfigLua = ''
        local originalNotify = vim.notify
        vim.notify = function (msg, log_level, optopts)
          -- TODO: only ignore the message about textDocument/documentColor
          -- originalNotify(msg, log_level, opts)
        end

        require('nu').setup{}

        require('nvim-surround').setup{}

        require('tiny-inline-diagnostic').setup({
          preset = "powerline";
          options = {
            multilines = {
              enable = true,
              always_show = true,
              trim_whitespace = false,
              tabstop = 4,
            };
            overflow = {
              mode = "wrap",
            },
            break_line = {
              enabled = false,
              after = 30,
            },
          },
        })
        vim.diagnostic.config({ virtual_text = false })

        require('blame').setup({
        })

        require('outline').setup({
          outline_window = {
            position = 'left',
          },
          preview_window = {
            auto_preview = true,
          },
        })

        require('FTerm').setup {}

        require('actions-preview').setup {
          highlight_command = {
            require('actions-preview.highlight').delta()
          },
          backend = { 'telescope' },
        }

      '';

      colorschemes.vscode = {
        enable = true;
        settings = {
          transparent = true;
          italic_commnts = true;
          underline_links = true;
          disable_nvimtree_bg = true;
          group_overrides = {
            StatusLine = {
              bg = "NONE";
            };
            MoreMsg = {
              bg = "NONE";
            };
            TabLine = {
              bg = "NONE";
            };
            TabLineSel = {
              bg = "NONE";
              underline = true;
              sp = "NvimLightYellow";
            };
            TabLineFill = {
              bg = "NONE";
            };
            PMenu = {
              bg = "NONE";
            };
            PMenuSel = {
              bg = "NONE";
              underline = true;
              sp = "NvimLightYellow";
            };
            "@function" = {
              fg = "#9c74ef";
            };
          };
        };
      };

      plugins = {
        ## Git
        neogit = {
          enable = true;

          settings = {
            graph_style = "unicode";
            disable_insert_on_commit = true;
            # console_timeout = 10000; # password input can be slow
            integrations = {
              telescope = true;
              diffview = false;
            };
          };
        };

        octo = {
          enable = true;
        };

        gitsigns = {
          enable = true;

          settings = {
            numhl = true;
            current_line_blame = true;
            current_line_blame_opts = {
              delay = 500;
              virt_text_pos = "right_align";
            };
          };
        };

        diffview = {
          enable = false; # kinda annoying, trying out git-conflict
          enhancedDiffHl = true;
        };

        ## Completion
        coq-nvim = {
          enable = false;
          installArtifacts = true;
          settings = {
            auto_start = true;
          };
        };

        cmp = {
          enable = true;
          autoEnableSources = true;
          settings = {
            sources = [
              { name = "async_path"; }
              { name = "cmp-dictionary"; }
              { name = "git"; }
              { name = "nvim_lsp"; }
              { name = "rg"; }
              { name = "spell"; }
              { name = "treesitter"; }
            ];

            window = {
              completion = {
                border = "rounded";
                scrollbar = false;
              };
              documentation = {
                border = "rounded";
              };
            };

            mapping = {
              "<C-j>" = "cmp.mapping.select_next_item()";
              "<C-k>" = "cmp.mapping.select_prev_item()";
              "<C-CR>" = "cmp.mapping.confirm({ select = false })";
              "<C-e>" = "cmp.mapping.close()";
            };
          };
        };

        ## Languages
        treesitter = {
          enable = true;
          # nixGrammars = true;

          grammarPackages = with pkgs.vimPlugins.nvim-treesitter.builtGrammars; [
            agda
            awk
            bash
            bibtex
            c
            c_sharp
            cmake
            commonlisp
            cpp
            css
            csv
            dhall
            dockerfile
            dot
            ebnf
            elixir
            erlang
            fennel
            fish
            gdscript
            gdshader
            git_config
            gitattributes
            gitcommit
            gitignore
            glsl
            go
            godot_resource
            gomod
            gpg
            haskell
            haskell_persistent
            html
            ini
            javascript
            jq
            json
            just
            latex
            lua
            make
            markdown
            markdown_inline
            mermaid
            muttrc
            nginx
            nim
            nix
            ocaml
            ocaml_interface
            perl
            purescript
            python
            racket
            rst
            rust
            scheme
            scss
            sql
            ssh_config
            strace
            toml
            tsx
            typescript
            vim
            vimdoc
            xcompose
            xml
            yaml
            yuck
            zathurarc
          ];

          settings = {
            # auto_install = true;
            # ensure_installed = "all";
            # sync_install = true;

            highlight = {
              enable = true;
              additional_vim_regex_highlighting = false;
            };

            incremental_selection = {
              enable = true;
              keymaps = {
                init_selection = "gsa";
                node_incremental = "gsl";
                node_decremental = "gsL";
                scope_incremental = "gsk";
              };
            };

            indent.enable = true;
          };
        };

        treesitter-context = {
          enable = true;
          settings = {
            max_lines = 8;
            trim_scope = "outer";
            mode = "cursor";
          };
        };

        treesitter-refactor = {
          enable = true;

          highlightDefinitions.enable = false;

          smartRename = {
            enable = true;
            keymaps.smartRename = "grr";
          };

          navigation = {
            enable = false;
            keymaps = {
              # gotoDefinition = "";
              # gotoDefinitionLspFallback = "";
              # listDefinitions = "";
              # listDefinitionsToc = "";
              # gotoNextUsage = "";
              # gotoPreviousUsage = "";
            };
          };
        };

        treesitter-textobjects = {
          enable = true;

          select = {
            enable = false;
          };

          swap = {
            enable = true;
            swapNext = {
              "]wf" = "@function.outer";
              "]wp" = "@parameter.inner";
            };
            swapPrevious = {
              "[wf" = "@function.outer";
              "[wp" = "@parameter.inner";
            };
          };

          move = {
            enable = true;
            gotoNext = {
              "]f" = "@function.outer";
            };
            gotoPrevious = {
              "[f" = "@function.outer";
            };
          };
        };

        lsp = {
          enable = true;

          keymaps = {
            diagnostic = {
              # "]d" = "goto_next";
              # "]D" = "goto_prev";
              # "[d" = "goto_prev";
              # "[D" = "goto_next";
            };

            lspBuf = {
              "gd" = "definition";
              "gr" = "references";
              "K" = "hover";
            };
          };

          servers = {
            hls = {
              enable = true;
              installGhc = false;
              package = null;
              cmd = [ "haskell-language-server-wrapper" "--logfile" "hls.log" "--debug" "--lsp" ]; # "--debug" ];
              extraOptions = {
                settings = {
                  haskell = {
                    formattingProvider = "fourmolu";
                    cabalFormattingProvider = "cabalfmt";
                  };
                };
              };
            };

            rust_analyzer = {
              enable = true;
              package = null;
              installRustc = false;
              installCargo = false;
            };

            ts_ls = {
              enable = true;
              package = null;
              rootMarkers = [ "package.json" "tsconfig.json" ];
            };

            gopls = {
              enable = true;
              package = null;
            };

            nushell = {
              enable = true;
            };

            nil_ls = {
              enable = true;
            };

            purescriptls = {
              enable = true;
              package = null;
              extraOptions = {
                settings.purescript = {
                  formatter = "purs-tidy";
                  addSpagoSources = true;
                };
              };
            };

          };

        };

        conform-nvim = {
          enable = true;
          settings = {

            formatters_by_ft = {
              haskell = {
                __unkeyed-1 = "fourmolu";
                __unkeyed-2 = "ormolu";
                stop_after_first = true;
              };
              go = [ "gofmt" ];
              nix = [ "nixpkgs_fmt" ];
              bash = [ "shellcheck" "shfmt" ];
              markdown = [ "mdsf" ];
              javascript = [ "prettier" ];
              javascriptreact = [ "prettier" ];
              typescript = [ "prettier" ];
              typescriptreact = [ "prettier" ];
              "javascript.tsx" = [ "prettier" ];
              "typescript.tsx" = [ "prettier" ];
              cabalproject = [ "cabal_fmt" ];
              purescript = [ "purs-tidy" ];
              rust = [ "rustfmt" ];
              "*" = [ "trim_whitespace" ];
            };

            format_on_save = {
              timeout_ms = 500;
              lsp_format = "fallback";
            };
          };
        };

        none-ls = {
          enable = true;
          enableLspFormat = false;
        };

        trouble = {
          enable = true;
          settings = {
            auto_refresh = true;
          };
        };

        lspsaga = {
          enable = false; # buggy :/
          symbolInWinbar = {
            enable = true;
          };
          lightbulb.enable = false;
        };

        ## misc
        ccc = {
          enable = true;
          settings = {
            highlighter = {
              auto_enable = true;
            };
          };
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

            completion = {
              nvim_cmp = true;
              min_chars = 2;
            };

            checkbox = {
              order = [ " " "x" ">" "~" ];
            };

          };
        };

        ## UI
        bufferline = {
          enable = true;
          settings.options = {
            mode = "buffers";
            themable = true;
            numbers = "none";
            style = "underline";
            separator_style = "thin";
            show_buffer_close_icons = false;
            show_closed_icon = false;
            always_show_bufferline = true;
            sort_by = "insert_at_end";
            # diagnostics = "nvim_lsp";
            indicator.style = "underline";
          };
        };

        lualine = {
          enable = true;

          settings = {
            options = {
              theme = "palenight";
            };
            sections = {
              lualine_a = [ "mode" "hostname" ];
              lualine_b = [ "branch" "diff" "diagnostics" ];
              lualine_c = [ "filename" ];
              lualine_x = [ "lsp_progress" "encoding" "fileformat" "filetype" ];
              lualine_y = [ "progress" ];
              lualine_z = [ "location" ];
            };
            inactive_sections = {
              lualine_a = [ ];
              lualine_b = [ ];
              lualine_c = [ "filename" ];
              lualine_x = [ "location" ];
              lualine_y = [ ];
              lualine_z = [ ];
            };
            extensions = [ "quickfix" ];
          };
        };

        indent-blankline = {
          enable = true;
        };

        telescope = {
          enable = true;

          extensions = {
            file-browser = {
              enable = true;
            };
            fzf-native = {
              enable = true;
            };
            manix = {
              enable = true;
            };
            media-files = {
              enable = true;
            };
            ui-select = {
              enable = true;
            };
            undo = {
              enable = true;
            };
          };

          keymaps = {
            "<leader><leader>" = "git_files";
            "<leader>fg" = "live_grep";
            "<leader>fb" = "buffers";
            "<leader>m" = "commands";
            "<leader>/" = "current_buffer_fuzzy_find";
            "/" = "current_buffer_fuzzy_find";
            "<leader>ff" = "fd";
            "<leader>fch" = "highlights";
            "<leader>fk" = "keymaps";
            "<leader>fj" = "jumplist";
            "<leader>fd" = "lsp_document_symbols";
            "<leader>fD" = "lsp_workspace_symbols";
            # "<leader>fr" = "lsp_references";
            "<leader>fm" = "marks";
            "<leader>fq" = "quickfix";
            "<leader>fu" = "undo";
            "<leader>fe" = "file_browser";
            "<leader>fE" = "file_browser path=%:p:h select_buffer=true";
          };
        };

        nvim-bqf = {
          enable = true;
        };

        leap = {
          enable = true;
        };

        oil = {
          enable = true;
        };

        which-key = {
          enable = true;

        };

        marks = {
          enable = true;
        };

        web-devicons = {
          enable = true;
        };

        bufdelete = {
          enable = true;
        };

        git-conflict = {
          enable = true;
        };
      };
    };
  };
}
