/****************************************************************************
  * Locale module
  *
  * Sets up the internationalisation default locale, some console defaults, the
  * font, and the time zone (configurable).
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.locale;
  nospace = str: lib.filter (c: c == " ") (lib.stringToCharacters str) == [ ];
  timezone = lib.types.nullOr (lib.types.addCheck lib.types.str nospace) // {
    description = "null or string without spaces";
  };

in
{
  imports = [ ];

  options.evie.locale = {
    timeZone = lib.mkOption {
      default = "Europe/Bucharest";
      type = timezone;
      example = "Europe/Bucharest";
      description = ''
        The time zone used when displaying times and dates. See <link
        xlink:href="https://en.wikipedia.org/wiki/List_of_tz_database_time_zones"/>
        for a comprehensive list of possible values for this setting.

        If null, the timezone will default to UTC and can be set imperatively
        using timedatectl.
      '';
    };
  };

  config = {
    i18n.defaultLocale = "en_US.UTF-8";

    console = {
      font = "Lat2-Terminus16";
      keyMap = "us";
    };

    fonts.packages = [ pkgs.nerdfonts ];

    time.timeZone = cfg.timeZone;
  };
}
