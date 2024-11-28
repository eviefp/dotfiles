{ lib, config, pkgs, dotfiles, osConfig, ... }:
let
  cfg = config.evie.programs.twitch-tui;
  settingsFormat = pkgs.formats.toml { };
  nuShellScript = dotfiles.self.packages.${pkgs.system}.nuShellScript;
  wrapper = pkg: nuShellScript {
    name = "twitch-tui";
    text = ''
      let secret = (cat ${osConfig.sops.secrets.twitchToken.path})
      with-env { TWT_TOKEN: $secret } { ${lib.getExe pkg} }
    '';
    runtimeInputs = [ pkg ];
  };
in
{
  options.evie.programs.twitch-tui = {
    enable = lib.mkEnableOption "twitch";
    package = lib.mkPackageOption pkgs "twitch-tui" { };
    settings = lib.mkOption {
      type = settingsFormat.type;
      example = lib.literalExpression ''
        '';
      default = {
        twitch = {
          username = "evie__ro";
          channel = "evie__ro";
          server = "irc.chat.twitch.tv";
          # The authentication token for the IRC.
          # Can be received here for default scopes: https://twitchapps.com/tmi/
          #   ["channel:moderate", "channel_editor", "chat:edit", "chat:read", "whispers:edit", "whispers:read"]
          # And here for custom scopes: https://twitchtokengenerator.com/
          #   "user:read:follows" to see who you are following
          token = "";
        };

        terminal = {
          # MS between updates
          delay = 30;
          maximum_messages = 500;
          log_file = "";
          verbose = false;
          first_state = "dashboard";
        };

        storage = {
          channels = true;
          mentions = true;
        };

        filters = {
          enabled = false;
          reversed = false;
        };

        frontend = {
          show_datetimes = true;
          datetime_format = "%a %b %e %T %Y";
          username_shown = true;
          palette = "warm";
          title_shown = true;
          margin = 0;
          badges = true;
          theme = "dark";
          state_tabs = true;
          cursor_shape = "block";
          blinking_cursor = false;
          inverted_scrolling = false;
          show_scroll_offset = true;
          twitch_emotes = true;
          bettertv_emotes = true;
          seventv_emotes = true;
          frankerfacez_emotes = true;
          favorite_channels = [ "evie__ro" "elmurorosa" "hasanabi" "AdmiralBahroo" "Catonmarz" ];
          recent_channel_count = 5;
          border_type = "rounded";
          hide_chat_border = false;
          right_align_usernames = false;
          show_unsupported_screen_size = true;
        };
      };
      description = ''
        Twitch-tui configuration.
        For available settings, see <>.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ (wrapper cfg.package) ];
    xdg.configFile."twt/config.toml".source = settingsFormat.generate "config.toml" cfg.settings;
  };
}
