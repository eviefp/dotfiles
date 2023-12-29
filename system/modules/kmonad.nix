/****************************************************************************
  * Kmonad
  **************************************************************************/
{ lib, config, kmonad, ... }:
let cfg = config.evie.kmonad;
in
{
  imports = [ ];

  options.evie.kmonad = {
    enable = lib.options.mkEnableOption "Enable kmonad.";
  };

  config = lib.mkIf cfg.enable {
    services.kmonad = {
      enable = true;

      # TODO: other systems?
      keyboards.moonlander = {
        device = "/dev/input/by-id/usb-ZSA_Moonlander_Mark_I-event-kbd";
        config = lib.readFile ../../config/kmonad/moonlander.kbd;
      };
    };

    services.xserver = {
      xkbOptions = "compose:caps";
      layout = "us";
    };
  };
}
