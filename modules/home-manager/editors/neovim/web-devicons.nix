{
  lib,
  config,
  ...
}: let
  cfg = config.evie.editors.neovim;
in {
  config = lib.mkIf cfg.enable {
    programs.nixvim = {
      plugins.web-devicons = {
        enable = true;
        settings = {
          default = true;
          variant = "dark";
        };
      };
    };
  };
}
