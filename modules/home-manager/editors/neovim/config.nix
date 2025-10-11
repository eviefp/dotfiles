{
  lib,
  config,
  ...
}: let
  cfg = config.evie.editors.neovim;
in {
  config = lib.mkIf cfg.enable {
    programs.nixvim = {
      globals = {
        mapleader = " ";
        maplocalleader = ",";
      };

      opts = {
        cmdheight = 0;
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

        # clipboard = "unnamedplus";

        showtabline = 2;

        # TODO: should these be moved to origami?
        conceallevel = 1;

        foldenable = true;
        foldlevel = 99;
        foldlevelstart = 99;
      };
    };
  };
}
