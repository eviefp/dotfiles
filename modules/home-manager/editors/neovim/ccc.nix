{...}: {
  config.programs.nixvim = {
    keymaps = [
      {
        key = "<leader>cc";
        action = "<cmd>CccPick<cr>";
        options.desc = "color picker";
      }
    ];

    plugins.ccc = {
      enable = true;
      settings = {
        highlighter = {
          auto_enable = true;
        };
      };
    };
  };
}
