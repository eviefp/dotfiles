{
  lib,
  config,
  pkgs,
  ...
}: let
  cfg = config.evie.editors.neovim;
in {
  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.haskellPackages.cabal-fmt
    ];
    programs.nixvim = {
      lsp.servers.hls = {
        enable = true;
        package = null;
        settings = {
          cmd = ["haskell-language-server-wrapper" "--logfile" "hls.log" "--debug" "--lsp"]; # "--debug" ];
          haskell = {
            formattingProvider = "fourmolu";
            cabalFormattingProvider = "cabalfmt";
          };
        };
      };

      plugins.conform-nvim.settings.formatters_by_ft = {
        haskell = {
          __unkeyed-1 = "fourmolu";
          __unkeyed-2 = "ormolu";
          stop_after_first = true;
        };
        cabalproject = ["cabal_fmt"];
      };
    };
  };
}
