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
      extraPlugins = [
        (pkgs.vimUtils.buildVimPlugin {
          pname = "blame.nvim";
          version = "v1.0";
          src = pkgs.fetchFromGitHub {
            owner = "FabijanZulj";
            repo = "blame.nvim";
            rev = "d5f4ad5c8ebca3caecc773dda5983709c5a5da8f";
            hash = "sha256-IP+KIwAANoRdZmSzAAtjLgeYOADc2t8Ld1ru0sNq/G0=";
          };
        }) # https://github.com/FabijanZulj/blame.nvim
      ];
      keymaps = [
        {
          key = "<leader>gb";
          action = "<cmd>BlameToggle<cr>";
          options.desc = "blame toggle";
        }
      ];
      extraConfigLua = ''
        require('blame').setup {}
      '';
    };
  };
}
