{...}: {
  config.programs.nixvim = {
    plugins.telescope = {
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
  };
}
