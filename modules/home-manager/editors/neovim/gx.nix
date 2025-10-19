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
            plugin = true;
            github = true;
            search = true;
            nixgh = {
              name = "nixgh";
              handle.__raw =
                /*
                lua
                */
                ''
                  function(mode, line, _)
                    local url = require('gx.helper').find(line, mode, 'github:([^/]+/[^/";]+)')
                    if url then
                      return "https://github.com/" .. url
                    end
                  end
                '';
            };
          };
        };
      };
    };
  };
}
