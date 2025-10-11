{...}: {
  config.programs.nixvim = {
    keymaps = [
      {
        key = "<leader>ac";
        action = "<cmd>lua require('actions-preview').code_actions()<cr>";
        options.desc = "lsp: code actions";
      }
    ];

    plugins.actions-preview = {
      enable = true;
      settings = {
        highlight_command.__raw =
          /*
          lua
          */
          ''
            {
              require('actions-preview.highlight').delta 'delta --side-by-side',
              require('actions-preview.highlight').diff_so_fancy(),
              require('actions-preview.highlight').diff_highlight(),
            }
          '';
      };
    };
  };
}
