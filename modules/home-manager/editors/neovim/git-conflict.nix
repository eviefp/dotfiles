{...}: {
  config.programs.nixvim = {
    plugins.git-conflict = {
      enable = true;
    };
  };
}
