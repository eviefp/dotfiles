{
  lib,
  config,
  pkgs,
  ...
}: let
  cfg = config.evie.editors.neovim;
in {
  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.mdsf

      pkgs.shellcheck
      pkgs.shfmt
    ];

    # NOTE: consider adding new configs to their own lang/ modules.
    programs.nixvim = {
      plugins.conform-nvim = {
        enable = true;
        settings = {
          formatters_by_ft = {
            bash = ["shellcheck" "shfmt"];
            markdown = ["mdsf"];
            "*" = ["trim_whitespace"];
          };

          format_on_save = {
            timeout_ms = 500;
            lsp_format = "fallback";
          };
        };
      };
    };
  };
}
