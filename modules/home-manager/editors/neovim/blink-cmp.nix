{...}: {
  config.programs.nixvim = {
    keymaps = [
    ];

    plugins = {
      blink-cmp = {
        enable = true;
        settings = {
          keymap = {
            preset = "default";
            "<C-space>" = ["show"];
          };

          appearance.nerd_font_variant = "mono";

          cmdline.enabled = true;

          completion = {
            documentation.auto_show = true;

            keyword.range = "full";

            ghost_text = {
              enabled = true;
            };

            menu.draw = {
              treesitter = ["lsp"];
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
    };
  };
}
