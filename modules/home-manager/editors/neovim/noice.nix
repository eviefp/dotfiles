{...}: {
  config = {
    programs.nixvim = {
      opts = {
        # no need for the cmd line if it's a popup
        cmdheight = 0;
      };

      plugins = {
        noice = {
          enable = true;
          settings = {
            cmdline = {
              enabled = true;
              view = "cmdline_popup";
            };
            messages = {
              enabled = true;
              view_error = "notify";
              view_warn = "notify";
              view_history = "messages";
              view_search = "virtualtext";
            };
            popupmenu = {
              enabled = true;
              backend = "nui";
            };
            notify = {
              enabled = true;
              view = "notify";
            };
            lsp = {
              enabled = false;
            };
            presets = {
              bottom_search = true;
              command_palette = true;
              long_message_to_split = true;
              inc_rename = false;
              lsp_doc_border = true;
            };
          };
        };

        nui.enable = true;

        notify = {
          enable = true;
          settings = {
            level = "warn";
            background_colour = "#000000";
            max_width = 30;
          };
        };
      };
    };
  };
}
