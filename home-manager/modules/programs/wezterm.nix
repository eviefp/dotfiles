/****************************************************************************
  * programs/wezterm module
  *
  * https://wezfurlong.org/wezterm
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.programs.wezterm;
in
{
  imports = [ ];

  options.evie.programs.wezterm = {
    enable = lib.options.mkEnableOption "Enable wezterm";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.wezterm
    ];
    home.file.".config/wezterm/wezterm.lua".text = ''
local wezterm = require 'wezterm'

return {
  default_prog = {"${pkgs.fish}/bin/fish"},
  -- launch_menu = {},
  font = wezterm.font('Hasklug Nerd Font Mono', { weight = "Regular", stretch = "Normal", italic = false }),
  check_for_updates = false,
  font_size = 10,
  -- color_scheme = 'Molokai',
  enable_tab_bar = false,
  window_background_opacity = 0.6,
  text_background_opacity = 1.0,
}
'';
  };
}
