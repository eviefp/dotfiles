{...}: {
  config = {
    programs.nixvim = {
      plugins.lean = {
        # temporarily disable lean LSP because I'm not really using it, but
        # I am abusing the lean4 syntax highlighting for my language, sorry
        enable = false;
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
    };
  };
}
