{...}: {
  config = {
    programs.nixvim = {
      diagnostic.settings = {
        virtual_lines = {
          only_current_line = true;
        };
      };

      keymaps = [
        {
          key = "<leader>dt";
          action = ":lua require('lsp_lines').toggle()<cr>";
          options.desc = "line diagnostics toggle";
        }
      ];

      plugins.lsp-lines = {
        enable = true;
      };
    };
  };
}
