{...}: {
  config = {
    programs.nixvim = {
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
