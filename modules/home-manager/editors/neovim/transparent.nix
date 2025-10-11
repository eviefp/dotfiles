{...}: {
  config.programs.nixvim = {
    plugins.transparent = {
      enable = true;
      settings = {
        extra_groups = [];
        exclude_grups = [];
      };
    };
  };
}
