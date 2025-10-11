{pkgs, ...}: {
  config = {
    home.packages = [
      pkgs.rustfmt
    ];

    programs.nixvim = {
      lsp.servers.rust_analyzer = {
        enable = true;
        package = null;
      };

      plugins.conform-nvim.settings.formatters_by_ft = {
        rust = ["rustfmt"];
      };
    };
  };
}
