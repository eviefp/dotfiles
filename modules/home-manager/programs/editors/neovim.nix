/****************************************************************************
  * Neovim module
  *
  * Neovim package, plugins, and init file.
  **************************************************************************/
{ dotfiles, pkgs, ... }:
{
  imports = [
    dotfiles.nixvim.homeManagerModules.nixvim
  ];

  config.home.packages = [
    pkgs.obsidian
    pkgs.codespell
    pkgs.shellcheck
    pkgs.shfmt
    pkgs.haskellPackages.cabal-fmt
    pkgs.yamlfmt
  ];

  config.home.sessionVariables.EDITOR = "nvim";

  config.programs.nixvim = {
    enable = true;

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
        key = "<leader>ac";
        action = "<cmd>lua vim.lsp.buf.code_action()<cr>";
      }
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
        action = "<cmd>bdelete<cr>";
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

    ];

    extraPlugins = [
      pkgs.vimPlugins.nvim-nu
      pkgs.vimPlugins.nvim-surround
      (pkgs.vimUtils.buildVimPlugin {
        inherit (pkgs.luaPackages.lua-utils-nvim) pname version src;
      })
      (pkgs.vimUtils.buildVimPlugin {
        inherit (pkgs.luaPackages.pathlib-nvim) pname version src;
      })
      (pkgs.vimUtils.buildVimPlugin {
        inherit (pkgs.luaPackages.nvim-nio) pname version src;
      })
      pkgs.vimPlugins.lualine-lsp-progress
      pkgs.vimPlugins.tiny-inline-diagnostic-nvim
    ];

    extraConfigLua = ''
      require('nu').setup{}

      require('nvim-surround').setup{}

      vim.diagnostic.config({ virtual_text = false })
      require('tiny-inline-diagnostic').setup({
      })
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
        nixGrammars = true;

        settings = {
          auto_install = true;
          ensure_installed = "all";
          sync_install = true;

          highlight = {
            enable = true;
            additional_vim_regex_highlighting = true;
          };

          incremental_selection.enable = true;

          indent.enable = true;
        };
      };

      lsp = {
        enable = true;

        keymaps = {
          diagnostic = {
            "]d" = "goto_next";
            "]D" = "goto_prev";
            "[d" = "goto_prev";
            "[D" = "goto_next";
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
            extraOptions = {
              settings = {
                haskell = {
                  formattingProvider = "ormolu";
                };
              };
            };
          };

          rust-analyzer = {
            enable = true;
            installRustc = false;
            installCargo = false;
          };

          purescriptls = {
            enable = true;
          };

          nushell = {
            enable = true;
          };

          nil-ls = {
            enable = true;
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
            yaml = [ "yamlfmt" ];
            rust = [ "rustfmt" ];
            "*" = [ "codespell" "trim_whitespace" ];
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

      ## misc
      ccc = {
        enable = true;
        settings = {
          highlighter = {
            auto_enable = true;
          };
        };
      };

      neorg = {
        enable = false;
        modules = {
          "core.defaults".__empty = null;
          "core.concealer".__empty = null;
          "core.dirman".config.workspaces = {
            notes = "~/code/neorg";
          };
          "core.summary".__empty = null;
          "core.text-objects".__empty = null;
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
          diagnostics = "nvim_lsp";
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
            lualine_a = ["mode" "hostname"];
            lualine_b = ["branch" "diff" "diagnostics"];
            lualine_c = ["filename"];
            lualine_x = ["lsp_progress" "encoding" "fileformat" "filetype"];
            lualine_y = ["progress"];
            lualine_z = ["location"];
          };
          inactive_sections = {
            lualine_a = [];
            lualine_b = [];
            lualine_c = ["filename"];
            lualine_x = ["location"];
            lualine_y = [];
            lualine_z = [];
          };
          extensions = [ "quickfix" ];
        };
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
          "<leader>ff" = "fd";
          "<leader>fe" = "diagnostics";
          "<leader>fch" = "highlights";
          "<leader>fk" = "keymaps";
          "<leader>fj" = "jumplist";
          "<leader>fd" = "lsp_document_symbols";
          "<leader>fD" = "lsp_workspace_symbols";
          "<leader>fr" = "lsp_references";
          "<leader>fm" = "marks";
          "<leader>fq" = "quickfix";
        };
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
    };
  };
}
