{...}: {
  config.programs.nixvim = {
    plugins.web-devicons = {
      enable = true;
      settings = {
        default = true;
        variant = "dark";
      };
    };
  };
}
