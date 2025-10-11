{
  lib,
  config,
  ...
}: let
  cfg = config.evie.editors.neovim;
in {
  config = lib.mkIf cfg.enable {
    programs.nixvim = {
      lsp.servers.omnisharp = {
        enable = true;
        package = null;
      };
    };
  };
}
