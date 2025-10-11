{...}: {
  config = {
    programs.nixvim = {
      keymaps = [
        {
          key = "gx";
          action = "<cmd>Browse<cr>";
          options.desc = "browse link";
        }
      ];

      plugins.gx = {
        enable = true;
        settings = {
          handlers = {
            github = {
              name = "github";
              handle.__raw =
                /*
                lua
                */
                ''
                  function(mode, line, _)
                    local url = require('gx.helper').find(line, mode, 'github:([^/]+/[^/";]+)')
                    return "https://github.com/" .. url
                  end
                '';
            };
          };
        };
      };
    };
  };
}
