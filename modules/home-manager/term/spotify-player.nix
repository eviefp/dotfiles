{ lib, config, pkgs, ... }:
let
  cfg = config.evie.programs.spotify;
  settingsFormat = pkgs.formats.toml { };
in
{
  options.evie.programs.spotify = {
    enable = lib.mkEnableOption "spotify";
    package = lib.mkPackageOption pkgs "spotify-player" { };
    settings = lib.mkOption {
      type = settingsFormat.type;
      example = lib.literalExpression ''
        '';
      description = ''
        Spotify-player configuration.
        For available settings, see <>.
      '';
    };
    keymap = lib.mkOption {
      type = settingsFormat.type;
      example = lib.literalExpression ''
        '';
      description = ''
        Spotify-player configuration.
        For available settings, see <>.
      '';
    };
    # theme = lib.mkOption {
    #   type = settingsFormat.type;
    #   example = lib.literalExpression ''
    #     '';
    #   description = ''
    #     Spotify-player configuration.
    #     For available settings, see <>.
    #   '';
    # };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];
    xdg.configFile."spotify-player/app.toml".source = settingsFormat.generate "app.toml" cfg.settings;
    # xdg.configFile."spotify-player/theme.toml".source = settingsFormat.generate "theme.toml" cfg.theme;
    xdg.configFile."spotify-player/keymap.toml".source = settingsFormat.generate "keymap.toml" cfg.keymap;
  };
}
