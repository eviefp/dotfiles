{...}: {
  config.programs.nixvim = {
    plugins.visual-multi = {
      enable = true;
      settings = {
        mouse_mappings = 0;
        silent_exit = 1;
        show_warnings = 1;
        default_mappings = 1;
      };
    };
  };
}
