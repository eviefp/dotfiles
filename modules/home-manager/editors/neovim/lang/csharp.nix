{...}: {
  config.programs.nixvim = {
    lsp.servers.omnisharp = {
      enable = true;
      package = null;
    };
  };
}
