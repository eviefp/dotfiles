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
      extraPlugins = [pkgs.vimPlugins.nvim-sops];

      keymaps = [
        {
          key = "<leader>se";
          action = "<cmd>SopsEncrypt<cr>";
          options.desc = "sops: encrypt";
        }
        {
          key = "<leader>sd";
          action = "<cmd>SopsDecrypt<cr>";
          options.desc = "sops: decrypt";
        }
      ];
    };
  };
}
