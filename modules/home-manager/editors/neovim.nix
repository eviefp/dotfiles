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

      # diagram
      pkgs.mermaid-cli
      pkgs.plantuml
      pkgs.d2
      pkgs.gnuplot
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

      diagnostic.settings = {
        virtual_lines = true;
      };

      globals = {
        mapleader = " ";
        maplocalleader = ",";
        markdown_recommended_style = 0;
      };

      opts = {
        cmdheight = 0;
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

        conceallevel = 1;

        foldenable = true;
        foldlevel = 99;
        foldlevelstart = 99;
      };

      keymaps = [
        {
          key = "\\";
          action = ":nohl<cr>";
          options.desc = "highlight clear";
        }
        {
          key = "<S-Tab>";
          action = ":set foldlevel=1<cr>";
          options.desc = "fold first level";
        }
        {
          key = "<A-Tab>";
          action = ":set foldlevel=99<cr>";
          options.desc = "un-fold all";
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
          options.desc = "path to clipboard";
        }
        {
          key = "<leader>wv";
          action = ":vsplit<cr>";
          options.desc = "split vertically";
        }
        {
          key = "<leader>ws";
          action = ":split<cr>";
          options.desc = "split horizontally";
        }
        {
          key = "<leader>ww";
          action = ":w<cr>";
          options.desc = "window: jump to next";
        }
        {
          key = "<leader>tt";
          action = ":vsplit<cr>:term<cr>a";
          options.desc = "term: vertical split";
        }
        {
          key = "<leader>ts";
          action = ":split<cr>:term<cr>a";
          options.desc = "term: horizontal split";
        }
        {
          key = "<esc>";
          action = "<C-\\><C-n>";
          mode = "t";
          options.desc = "term: normal mode";
        }
        ## lsp
        {
          key = "gD";
          action = ":split<cr>:lua vim.lsp.buf.definition()<cr>";
          options.desc = "lsp definition: horizontal split";
        }
        {
          key = "<leader>ac";
          action = "<cmd>lua require('actions-preview').code_actions()<cr>";
          options.desc = "lsp: code actions";
        }
        # blame
        {
          key = "<leader>gb";
          action = "<cmd>BlameToggle<cr>";
          options.desc = "blame toggle";
        }
        # Neogit
        {
          key = "<leader>gs";
          action = "<cmd>Neogit cwd=%:p:h<cr>";
          options.desc = "neogit UI";
        }
        # gitsigns
        {
          key = "]g";
          action = "<cmd>Gitsigns next_hunk<cr>";
          options.desc = "goto next git hunk";
        }
        {
          key = "[g";
          action = "<cmd>Gitsigns prev_hunk<cr>";
          options.desc = "goto prev git hunk";
        }
        {
          key = "<leader>gt";
          action = "<Cmd>Gitsigns toggle_current_line_blame<cr>";
          options.desc = "blame: current line toggle";
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
          key = "<leader>bc";
          action = "<cmd>BufferClose<cr>";
          options.desc = "buffer: close";
        }
        {
          key = "<leader>bC";
          action = "<cmd>BufferLineCloseAllButCurrent<cr>";
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
        # ccc
        {
          key = "<leader>cc";
          action = "<cmd>CccPick<cr>";
          options.desc = "color picker";
        }
        # sops
        {
          key = "<leader>ce";
          action = "<cmd>SopsEncrypt<cr>";
          options.desc = "sops: encrypt";
        }
        {
          key = "<leader>cd";
          action = "<cmd>SopsDecrypt<cr>";
          options.desc = "sops: decrypt";
        }
        # tabularize
        {
          key = "<leader>ta";
          action = ":Tabularize ";
          options.desc = "custom align";
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
        # outline
        {
          key = "<leader>oo";
          action = ":Outline<cr>";
          options.desc = "outline";
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
      ];

      extraPlugins = [
        pkgs.vimPlugins.nvim-nu
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
        -- local originalNotify = vim.notify
        -- vim.notify = function (msg, log_level, optopts)
        --   -- TODO: only ignore the message about textDocument/documentColor
        --   -- originalNotify(msg, log_level, opts)
        -- end

        require('nu').setup{}

        require('blame').setup({
        })

        require('outline').setup({
          outline_window = {
            position = 'left',
            auto_close = true,
          },
          preview_window = {
            auto_preview = true,
          },
        })

        require('actions-preview').setup {
          highlight_command = {
            require('actions-preview.highlight').delta()
          },
          backend = { 'telescope' },
        }
      '';

      # TODO: move these into some default system colors.
      #
      # M.default = {
      #     bg = "#16181a",
      #     bg_alt = "#1e2124",
      #     bg_highlight = "#3c4048",
      #     fg = "#ffffff",
      #     grey = "#7b8496",
      #     blue = "#5ea1ff",
      #     green = "#5eff6c",
      #     cyan = "#5ef1ff",
      #     red = "#ff6e5e",
      #     yellow = "#f1ff5e",
      #     magenta = "#ff5ef1",
      #     pink = "#ff5ea0",
      #     orange = "#ffbd5e",
      #     purple = "#bd5eff",
      # }
      #
      # M.light = {
      #     bg = "#ffffff",
      #     bg_alt = "#eaeaea",
      #     bg_highlight = "#acacac",
      #     fg = "#16181a",
      #     grey = "#7b8496",
      #     blue = "#0057d1",
      #     green = "#008b0c",
      #     cyan = "#008c99",
      #     red = "#d11500",
      #     yellow = "#997b00",
      #     magenta = "#d100bf",
      #     pink = "#f40064",
      #     orange = "#d17c00",
      #     purple = "#a018ff",
      # }
      colorschemes.cyberdream = {
        enable = true;
        settings = {
          variant = "dark";
          transparent = true;
          saturation = 1;
          italic_comments = true;
          borderless_pickers = false;
          terminal_colors = true;
          highlights = {
            Comment = {
              fg = "pink";
              italic = true;
            };
            Character = {
              fg = "orange";
            };
            String = {
              fg = "orange";
            };
            TreeSitterContext = {
              fg = "NONE";
              bg = "NONE";
            };
            TreeSitterContextLineNumber = {
              bg = "purple";
            };
            TreesitterContextBottom = {
              underline = true;
              fg = "NONE";
              bg = "NONE";
              sp = "purple";
            };
          };
        };
      };

      plugins = {
        ## Git
        neogit = {
          enable = true;

          settings = {
            graph_style = "kitty";
            disable_insert_on_commit = true;
            integrations = {
              telescope = true;
              diffview = false;
            };
          };
        };

        gitsigns = {
          enable = true;

          settings = {
            signs_staged_enable = true;
            signcolumn = true;
            numhl = true;
            auto_attach = true;
            attach_to_untracked = false;
            current_line_blame = false;
            current_line_blame_opts = {
              virt_text = true;
              virt_text_pos = "right_align";
              delay = 500;
              ignore_whitespace = false;
            };
          };
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
            formatting.format.__raw = ''
              function(entry, vim_item)
                local highlights_info = require("colorful-menu").cmp_highlights(entry)

                -- highlight_info is nil means we are missing the ts parser, it's
                -- better to fallback to use default `vim_item.abbr`. What this plugin
                -- offers is two fields: `vim_item.abbr_hl_group` and `vim_item.abbr`.
                if highlights_info ~= nil then
                    vim_item.abbr_hl_group = highlights_info.highlights
                    vim_item.abbr = highlights_info.text
                end

                return vim_item
              end
            '';
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
          nixGrammars = true;
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
            regex
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
            max_lines = 12;
            trim_scope = "outer";
            mode = "cursor";
          };
        };

        treesitter-refactor = {
          enable = true;

          highlightDefinitions.enable = false;
          highlightCurrentScope.enable = false;

          smartRename = {
            enable = true;
            keymaps.smartRename = "grr";
          };

          navigation = {
            enable = true;
            keymaps = {
              gotoDefinitionLspFallback = "gd";
              listDefinitions = "gnd";
              listDefinitionsToc = "gO";
              gotoNextUsage = "gnj";
              gotoPreviousUsage = "gnk";
            };
          };
        };

        # TODO: look into this
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

          luaConfig.post = ''
            vim.lsp.enable('omnisharp')
          '';

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

            omnisharp = {
              enable = true;
              package = null;
              cmd = [ "OmniSharp" ];
              filetypes = [ "cs" ];
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

            nil_ls = {
              enable = true;
            };

            gopls = {
              enable = true;
              package = null;
            };


            nushell = {
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

        ## misc
        ccc = {
          enable = true;
          settings = {
            highlighter = {
              auto_enable = true;
            };
          };
        };

        ## UI

        lualine = {
          enable = true;

          settings = {
            options = {
              theme = "palenight";
            };
            winbar = {
              lualine_a = [
                {
                  __unkeyed-1 = "filename";
                  path = 1;
                  shortening_target = 0;
                }
              ];
              lualine_b = [ ];
              lualine_c = [
              ];
              lualine_x = [ ];
              lualine_y = [ ];
              lualine_z = [ ];
            };
            inactive_winbar = {
              lualine_a = [
                {
                  __unkeyed-1 = "filename";
                  path = 1;
                  shortening_target = 0;
                }
              ];
              lualine_b = [ ];
              lualine_c = [
              ];
              lualine_x = [ ];
              lualine_y = [ ];
              lualine_z = [ ];
            };
            sections = {
              lualine_a = [ "mode" "hostname" ];
              lualine_b = [ "branch" "diff" "diagnostics" ];
              lualine_c = [ "filename" ];
              lualine_x = [ ];
              lualine_y = [
                "encoding"
                "fileformat"
              ];
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
            "<leader>/" = "current_buffer_fuzzy_find";
            # "/" = "current_buffer_fuzzy_find";
            "<leader>ff" = "fd";
            "<leader>fch" = "highlights";
            "<leader>fk" = "keymaps";
            "<leader>fj" = "jumplist";
            "<leader>fd" = "lsp_document_symbols";
            "<leader>fD" = "lsp_workspace_symbols";
            "<leader>fr" = "lsp_references";
            "<leader>fm" = "commands";
            "<leader>f'" = "marks";
            "<leader>fq" = "quickfix";
            "<leader>fu" = "undo";
            "<leader>fe" = "file_browser";
            "<leader>fE" = "file_browser path=%:p:h select_buffer=true";
            "<leader>fi" = "nerdy";
          };

          settings = {
            pickers.colorscheme.enable_preview = true;
            defaults.mappings = {
              i = {
                "<C-t>" = "open_with_trouble";
              };
              n = {
                "<C-t>" = "open_with_trouble";
              };
            };
          };

          luaConfig.post = ''
            require('telescope.actions').open_with_trouble = require('trouble.sources.telescope').open
          '';
        };

        leap = {
          enable = true;
        };

        marks = {
          enable = true;
        };

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
            standard_widths = [ 2 4 ];
          };
        };

        rainbow-delimiters.enable = true;

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

        # Color completion menus.
        # TODO: use it with blink https://github.com/xzbdmw/colorful-menu.nvim
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

        # Floating terminal.
        # Can also add multiple terminals and navigate through them.
        floaterm = {
          enable = true;
          settings = {
            shell = "nu";
            position = "bottom";
            keymap_new = "<A-T>";
            keymap_toggle = "<A-t>";
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
              chars = [ "│" "¦" "┆" "┊" ];
            };
            line_num = {
              enable = true;
              style = "#806d9c";
            };
            blank = {
              enable = true;
              chars = [ " " "․" "⁚" "⁖" "⁘" "⁙" ];
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

        # TODO: enable when switching cmp for blink
        # lspkind = {
        #   enable = true;
        #   settings = {
        #     mode = "symbol_text";
        #   };
        # };

        # Nicer markdown.
        markview = {
          enable = true;
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
              enabled = false;
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
          };
        };

        # surround; ys/yS
        nvim-surround = {
          enable = true;
        };

        # TODO: blink snacks
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
              nvim_cmp = true;
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
              order = [ " " ">" "x" "~" ];
            };
          };
        };

        # edit files in a buffer
        oil = {
          enable = true;
          settings = {
            default_file_explorer = true;
            columns = [ "icon" "permissions" "size" "mtime" ];
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
              setup = true;
              hOnlyOpensOnFirstColumn = false;
            };
          };
        };

        # REST API helper; use :Rest on a .http file
        rest = {
          enable = true;
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

        # travel through versions of a file; start with ':Tardis'
        # TODO: review if I really want this. It's not exactly blame, but it can git travel.
        tardis = {
          enable = true;
          settings = {
            keymap = {
              next = "<C-j>";
              prev = "<C-k>";
              quit = "q";
              revision_message = "<C-m>";
              commit = "<C-g>";
            };
            settings = {
              initial_revisions = 10;
              max_revisions = 256;
              show_commit_index = true;
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
            extra_groups = [ ];
            exclude_grups = [ ];
          };
        };

        trouble = {
          enable = true;
          package = pkgs.vimPlugins.trouble-nvim.overrideAttrs (previousAttrs: {
            patches =
              (previousAttrs.patches or [ ])
              ++ [ ./neovim/0001-fix-view-adapt-to-the-changes-for-nvim_set_decoratio.patch ];
          });
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
            win = {
              wo = {
                winblend = 50;
              };
            };
          };
        };

      };
    };
  };
}

