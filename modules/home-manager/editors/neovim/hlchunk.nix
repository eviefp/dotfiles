{theme, ...}: {
  config = {
    programs.nixvim = {
      plugins.hlchunk = {
        enable = true;
        settings = {
          chunk = {
            enable = true;
            use_treesitter = true;
          };
          indent = {
            enable = true;
            chars = ["│" "¦" "┆" "┊"];
          };
          line_num = {
            enable = true;
            style = "${theme.dark.purple}";
          };
          blank = {
            enable = true;
            chars = [" " "․" "⁚" "⁖" "⁘" "⁙"];
          };
        };
      };
    };
  };
}
