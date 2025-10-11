{...}: {
  config.programs.nixvim = {
    plugins.oil = {
      enable = true;
      settings = {
        default_file_explorer = true;
        columns = ["icon" "permissions" "size" "mtime"];
        view_options = {
          show_hidden = true;
        };
      };
    };
  };
}
