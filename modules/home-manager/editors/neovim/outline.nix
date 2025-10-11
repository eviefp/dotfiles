{pkgs, ...}: {
  config.programs.nixvim = {
    extraPlugins = [pkgs.vimPlugins.outline-nvim];
    keymaps = [
      {
        key = "<leader>oo";
        action = ":Outline<cr>";
        options.desc = "outline";
      }
    ];

    extraConfigLua = ''
      require('outline').setup{
        outline_window = {
          position = 'left',
          auto_close = true,
        },
        preview_window = {
          auto_preview = true,
        },
      }
    '';
  };
}
