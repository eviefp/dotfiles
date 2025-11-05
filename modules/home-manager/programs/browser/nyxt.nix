{
  lib,
  config,
  ...
}: let
  cfg = config.evie.programs.browser.nyxt;
in {
  options.evie.programs.browser.nyxt = {
    enable = lib.mkEnableOption "nyxt defaults";
  };

  config = lib.mkIf cfg.enable {
    programs.nyxt = {
      enable = true;
      # TODO: config = ./nyxt.el;
    };
  };
}
