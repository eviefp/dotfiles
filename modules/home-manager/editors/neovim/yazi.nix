{...}: {
  config.programs.nixvim = {
    keymaps = [
      {
        key = "<leader>ya";
        action = "<cmd>Yazi toggle<cr>";
        options.desc = "yazi toggle";
      }
    ];

    plugins.yazi = {
      enable = true;
    };
  };
}
