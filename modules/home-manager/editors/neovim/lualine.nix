{...}: {
  config.programs.nixvim = {
    plugins.lualine = {
      enable = true;

      settings = {
        options = {
          theme = "auto";
        };
        winbar = {
          lualine_a = [
            {
              __unkeyed-1 = "filename";
              path = 1;
              shortening_target = 0;
            }
          ];
          lualine_b = [];
          lualine_c = [
          ];
          lualine_x = [];
          lualine_y = [];
          lualine_z = [];
        };
        inactive_winbar = {
          lualine_a = [
            {
              __unkeyed-1 = "filename";
              path = 1;
              shortening_target = 0;
            }
          ];
          lualine_b = [];
          lualine_c = [
          ];
          lualine_x = [];
          lualine_y = [];
          lualine_z = [];
        };
        sections = {
          lualine_a = ["mode" "hostname"];
          lualine_b = ["branch" "diff" "diagnostics"];
          lualine_c = ["filename"];
          lualine_x = [];
          lualine_y = [
            "encoding"
            "fileformat"
          ];
          lualine_z = ["location"];
        };
        inactive_sections = {
          lualine_a = [];
          lualine_b = [];
          lualine_c = ["filename"];
          lualine_x = ["location"];
          lualine_y = [];
          lualine_z = [];
        };
        extensions = ["quickfix"];
      };
    };
  };
}
