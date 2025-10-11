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
      extraPlugins = [pkgs.vimPlugins.tabular];

      keymaps = [
        {
          key = "<leader>ta";
          action = ":Tabularize ";
          options.desc = "custom align";
        }
      ];
    };
  };
}
