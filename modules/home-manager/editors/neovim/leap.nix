{
  lib,
  config,
  ...
}: let
  cfg = config.evie.editors.neovim;
in {
  config = lib.mkIf cfg.enable {
    programs.nixvim = {
      plugins.leap = {
        enable = true;
      };
    };
  };
}
