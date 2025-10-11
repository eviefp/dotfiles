{pkgs, ...}: {
  config = {
    home.packages = [
      pkgs.nodePackages.purs-tidy
    ];

    programs.nixvim = {
      autoCmd = [
        {
          event = ["BufEnter"];
          pattern = ["*.purs"];
          command = ":setlocal filetype=purescript";
        }
      ];

      lsp.servers.purescriptls = {
        enable = true;
        package = null;
        settings = {
          purescript = {
            formatter = "purs-tidy";
            addSpagoSources = true;
          };
        };
      };

      plugins.conform-nvim.settings.formatters_by_ft = {
        purescript = ["purs-tidy"];
      };
    };
  };
}
