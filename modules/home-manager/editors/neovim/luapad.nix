{pkgs, ...}: {
  config.programs.nixvim = {
    extraPlugins = [
      pkgs.vimPlugins.nvim-luapad
    ];

    keymaps = [
      {
        key = "<leader>lp";
        action = "<cmd>Luapad<cr>";
        options.desc = "new interactive scratch buffer";
      }
      {
        key = "<leader>lr";
        action = "<cmd>LuaRun<cr>";
        options.desc = "run current buffer in a new scope";
      }
    ];

    extraConfigLua = ''
      require('luapad').setup {}
    '';
  };
}
