{
  lib,
  pkgs,
  config,
  ...
}: let
  cfg = config.evie.editors.neovim;
in {
  config = lib.mkIf cfg.enable {
    programs.nixvim = {
      extraPlugins = [pkgs.vimPlugins.nvim-nu];
      extraConfigLua = ''
        require('nu').setup {}
      '';

      lsp.servers.nushell.enable = true;
    };
  };
}
