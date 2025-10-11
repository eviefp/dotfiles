{...}: {
  config.programs.nixvim = {
    lsp.servers.gopls = {
      enable = true;
      package = null;
    };
  };

  plugins.conform-nvim.settings.formatters_by_ft = {
    go = ["gofmt"];
  };
}
