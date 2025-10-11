{...}: {
  config.programs.nixvim = {
    globals = {
      mapleader = " ";
      maplocalleader = ",";
    };

    opts = {
      number = true;
      relativenumber = true;

      splitright = true;
      splitbelow = true;

      shiftwidth = 2;
      shiftround = true;
      expandtab = true;

      termguicolors = true;

      autoread = true;
      backup = false;
      swapfile = true;

      visualbell = false;
      errorbells = false;

      clipboard = "unnamedplus";
    };
  };
}
