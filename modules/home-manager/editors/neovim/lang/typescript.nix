{pkgs, ...}: {
  config = {
    home.packages = [
      pkgs.prettier
    ];

    programs.nixvim = {
      lsp.servers.ts_ls = {
        enable = true;
        package = null;
        settings = {
          rootMarkers = ["package.json" "tsconfig.json"];
        };
      };

      plugins.conform-nvim.settings.formatters_by_ft = {
        javascript = ["prettier"];
        javascriptreact = ["prettier"];
        typescript = ["prettier"];
        typescriptreact = ["prettier"];
        "javascript.tsx" = ["prettier"];
        "typescript.tsx" = ["prettier"];
      };
    };
  };
}
