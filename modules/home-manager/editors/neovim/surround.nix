{...}: {
  config = {
    programs.nixvim = {
      plugins.nvim-surround.enable = true;
    };
  };
}
