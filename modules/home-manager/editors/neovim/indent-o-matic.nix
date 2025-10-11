{
  lib,
  config,
  ...
}: let
  cfg = config.evie.editors.neovim;
in {
  config = lib.mkIf cfg.enable {
    programs.nixvim = {
      plugins.indent-o-matic = {
        enable = true;
        settings = {
          skip_multiline = true;
          standard_widths = [2 4];
        };
      };
    };
  };
}
