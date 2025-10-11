{...}: {
  config = {
    programs.nixvim = {
      keymaps = [
        # Unbind arrow keys.
        {
          key = "<up>";
          action = "";
          mode = "n";
        }
        {
          key = "<up>";
          action = "";
          mode = "i";
        }
        {
          key = "<down>";
          action = "";
          mode = "n";
        }
        {
          key = "<down>";
          action = "";
          mode = "i";
        }
        {
          key = "<left>";
          action = "";
          mode = "n";
        }
        {
          key = "<left>";
          action = "";
          mode = "i";
        }
        {
          key = "<right>";
          action = "";
          mode = "n";
        }
        {
          key = "<right>";
          action = "";
          mode = "i";
        }

        {
          key = "\\";
          action = ":nohl<cr>";
          options.desc = "highlight clear";
        }

        # Folding.
        # TODO: consider moving to origami.
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

        # Window
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

        # Term
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

        {
          key = "<leader>fx";
          action = ":let @+ = expand(\"%\")<cr>";
          options.desc = "path to clipboard";
        }

        {
          key = "<leader>yy";
          action = ":w !pandoc -t html -F mermaid-filter | wl-copy --type text/html<cr>";
          options.desc = "copy html version of current file";
        }
        {
          key = "<leader>yv";
          action = ":'<,'>w !pandoc -t html -F mermaid-filter | wl-copy --type text/html<cr>";
          options.desc = "copy html version of current file";
          mode = "v";
        }
      ];
    };
  };
}
