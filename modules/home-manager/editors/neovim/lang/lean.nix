{...}: {
  config = {
    programs.nixvim = {
      plugins.lean = {
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
    };
  };
}
