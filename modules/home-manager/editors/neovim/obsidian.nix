{
  lib,
  config,
  pkgs,
  ...
}: {
  config = lib.mkIf config.evie.editors.neovim.obsidian {
    home.packages = [
      pkgs.obsidian
    ];

    programs.nixvim = {
      keymaps = [
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
      ];

      plugins.obsidian = {
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
    };
  };
}
