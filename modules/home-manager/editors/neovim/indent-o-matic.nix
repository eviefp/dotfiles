{...}: {
  config.programs.nixvim = {
    plugins.indent-o-matic = {
      enable = true;
      settings = {
        skip_multiline = true;
        standard_widths = [2 4];
      };
    };
  };
}
