{pkgs, ...}: {
  config = {
    home.packages = [
      pkgs.nufmt
    ];
    programs.nixvim = {
      extraPlugins = [pkgs.vimPlugins.nvim-nu];

      extraConfigLua = ''
        require('nu').setup {}
      '';

      lsp.servers.nushell.enable = true;

      plugins.conform-nvim.settings.formatters_by_ft = {
        nu = ["nufmt"];
      };
    };
  };
}
