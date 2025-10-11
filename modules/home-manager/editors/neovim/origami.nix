{...}: {
  config.programs.nixvim = {
    keymaps = [
      {
        key = "<C-h>";
        action = ":lua require('origami').h()<cr>";
        options.desc = "fold region";
      }
      {
        key = "<C-l>";
        action = ":lua require('origami').l()<cr>";
        options.desc = "un-fold region";
      }
    ];

    opts = {
      conceallevel = 1;

      foldenable = true;
      foldlevel = 99;
      foldlevelstart = 99;
    };

    plugins.origami = {
      enable = true;
      settings = {
        useLspFoldsWithTreesitterFallback = true;
        pauseFoldsOnSearch = true;
        foldtext = {
          enabled = true;
          padding = 2;
        };
        autoFold = {
          enabled = false;
        };
        foldKeymaps = {
          setup = false;
          hOnlyOpensOnFirstColumn = false;
        };
      };
    };
  };
}
