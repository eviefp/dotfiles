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
    dotfiles.nixvim.homeManagerModules.nixvim
  ];

  options.evie.editors.neovim = {
    enable = lib.mkEnableOption "neovim defaults";
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
          key = "<leader>fn";
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
          action = "<cmd>Lspsaga code_action<cr>";
        }
        {
          key = "<leader>fi";
          action = "<cmd>Lspsaga incoming_calls<cr>";
        }
        {
          key = "<leader>fo";
          action = "<cmd>Lspsaga outgoing_calls<cr>";
        }
        {
          key = "]d";
          action = "<cmd>Lspsaga diagnostic_jump_next<cr>";
        }
        {
          key = "[d";
          action = "<cmd>Lspsaga diagnostic_jump_prev<cr>";
        }
        {
          key = "<leader>fr";
          action = "<cmd>Lspsaga finder<cr>";
        }
        {
          key = "<C-t>";
          action = "<cmd>lua require('FTerm').toggle()<cr>";
        }
        # {
        #   key = "<C-o>";
        #   action = "<cmd>Lspsaga term_toggle<cr>";
        # }
        # {
        #   key = "<leader>fm";
        #   action = "<cmd>lua vim.lsp.buf.format()<cr>";
        # }
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

      ];

      extraPlugins = [
        pkgs.vimPlugins.nvim-nu
        pkgs.vimPlugins.nvim-surround
        (pkgs.vimUtils.buildVimPlugin {
          inherit (pkgs.luaPackages.lua-utils-nvim) pname version src;
        }) # same
        (pkgs.vimUtils.buildVimPlugin {
          inherit (pkgs.luaPackages.pathlib-nvim) pname version src;
        }) # same as below
        (pkgs.vimUtils.buildVimPlugin {
          inherit (pkgs.luaPackages.nvim-nio) pname version src;
        }) # ???, probably needed by something else?
        pkgs.vimPlugins.lualine-lsp-progress
        pkgs.vimPlugins.tiny-inline-diagnostic-nvim # nicer diagnostic, overlaps with ]d a bit
        (pkgs.vimUtils.buildVimPlugin {
          pname = "blame.nvim";
          version = "v1.0";
          src = pkgs.fetchFromGitHub {
            owner = "FabijanZulj";
            repo = "blame.nvim";
            rev = "39bb4cda9e658d053a43919b10ca52c0a4d738e1";
            hash = "sha256-9eI+4nv9vu0BlsuUk9n0d0k4jY4tu1RRO4yqItKwBkQ=";
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
            rev = "db0debbde453c0ce10d1b925cd9f3139e4291c42";
            hash = "sha256-ItUjSbuTNUSRKV3Ei77y4jH6hPJQNMP1bpcmv7GtTR8=";
          };
        }) # :CoAuthor
        pkgs.vimPlugins.litee-nvim # needed by gh.nvim
        (pkgs.vimUtils.buildVimPlugin {
          pname = "gh.nvim";
          version = "v1.0";
          src = pkgs.fetchFromGitHub {
            owner = "ldelossa";
            repo = "gh.nvim";
            rev = "ebbaac254ef7dd6f85b439825fbce82d0dc84515";
            hash = "sha256-5MWv/TpJSJfPY3y2dC1f2T/9sP4wn0kZ0Sed5OOFM5c=";
          };
        }) # :Gh
        pkgs.vimPlugins.nvim-sops
        pkgs.vimPlugins.tabular # :Tabularize
      ];

      extraConfigLua = ''
        require('nu').setup{}

        require('nvim-surround').setup{}

        require('tiny-inline-diagnostic').setup({
          options = {
            overflow = {
              mode = "oneline",
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

        require('litee.lib').setup {}
        require('litee.gh').setup {}
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
            console_timeout = 10000; # password input can be slow
            # integrations = {
            #   telescope = true;
            #   diffview = true;
            # };
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
          enable = true;
        };

        ## Completion
        coq-nvim = {
          enable = true;
          installArtifacts = true;
          settings = {
            auto_start = true;
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
            norg
            ocaml
            ocaml_interface
            org
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
              extraOptions = {
                settings = {
                  haskell = {
                    formattingProvider = "ormolu";
                  };
                };
              };
            };

            rust_analyzer = {
              enable = true;
              installRustc = false;
              installCargo = false;
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
                root_dir.__raw = "require('lspconfig').util.root_pattern('spago.dhall')";
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
              typescript = [ "prettier" ];
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

        lspsaga = {
          enable = true;
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

        obsidian = {
          enable = true;
          settings = {
            workspaces = [
              {
                name = "notes";
                path = "~/code/notes";
              }
            ];

            completion = {
              nvim_cmp = false;
              min_chars = 2;
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
              settings.hijack_netrw = false;
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
            "<leader>fe" = "diagnostics";
            "<leader>fch" = "highlights";
            "<leader>fk" = "keymaps";
            "<leader>fj" = "jumplist";
            "<leader>fd" = "lsp_document_symbols";
            "<leader>fD" = "lsp_workspace_symbols";
            # "<leader>fr" = "lsp_references";
            "<leader>fm" = "marks";
            "<leader>fq" = "quickfix";
            "<leader>fu" = "undo";
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
      };
    };
  };
}