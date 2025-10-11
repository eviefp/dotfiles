{...}: {
  config.programs.nixvim = {
    plugins.spider = {
      enable = true;
      keymaps = {
        silent = true;
        motions = {
          w = "w";
          e = "e";
          b = "b";
        };
      };
    };
  };
}
