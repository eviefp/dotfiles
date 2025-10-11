{pkgs, ...}: {
  config.programs.nixvim = {
    extraPlugins = [pkgs.vimPlugins.tabular];

    keymaps = [
      {
        key = "<leader>ta";
        action = ":Tabularize ";
        options.desc = "custom align";
      }
    ];
  };
}
