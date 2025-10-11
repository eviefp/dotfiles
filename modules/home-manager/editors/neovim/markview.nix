{...}: {
  config = {
    programs.nixvim = {
      autoCmd = [
        {
          event = ["BufEnter"];
          pattern = ["*.mdc"];
          command = ":setlocal filetype=markdown";
        }
      ];

      keymaps = [
        {
          key = "<leader>mv";
          action = "<cmd>Markview Toggle<cr>";
          options.desc = "markview toggle";
        }
      ];

      plugins.markview = {
        enable = true;
        settings = {
          preview.map_gx = false;
          markdown = {
            enable = true;
            list_items = {
              enable = true;
            };
          };
        };
      };
    };
  };
}
