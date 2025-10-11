{
  lib,
  config,
  theme,
  ...
}: let
  cfg = config.evie.editors.neovim;
in {
  config = lib.mkIf cfg.enable {
    programs.nixvim = {
      colorschemes.cyberdream = {
        enable = true;
        settings = {
          variant = "dark";
          transparent = true;
          saturation = 1;
          italic_comments = true;
          borderless_pickers = false;
          terminal_colors = true;
          highlights = {
            Comment = {
              fg = "${theme.dark.green}";
              italic = true;
            };
            Character = {
              fg = "${theme.dark.orange}";
            };
            String = {
              fg = "${theme.dark.orange}";
            };
            Keyword = {
              fg = "${theme.dark.blue}";
            };
            Function = {
              fg = "${theme.dark.pink}";
            };
            TreeSitterContext = {
              fg = "NONE";
              bg = "NONE";
            };
            TreeSitterContextLineNumber = {
              bg = "purple";
            };
            TreesitterContextBottom = {
              underline = true;
              fg = "NONE";
              bg = "NONE";
              sp = "${theme.dark.purple}";
            };
          };
        };
      };
    };
  };
}
