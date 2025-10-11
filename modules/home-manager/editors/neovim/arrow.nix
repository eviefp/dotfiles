{...}: {
  config.programs.nixvim = {
    plugins.arrow = {
      enable = true;
      settings = {
        show_icons = true;
        always_show_path = true;
        separate_by_branch = true;
        leader_key = ";";
      };
    };
  };
}
