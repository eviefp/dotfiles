{
  lib,
  config,
  pkgs,
  ...
}: let
  cfg = config.evie.term.spotify;
  settingsFormat = pkgs.formats.toml {};
in {
  options.evie.term.spotify = {
    enable = lib.mkEnableOption "spotify";
    package = lib.mkPackageOption pkgs "spotify-player" {};
    settings = lib.mkOption {
      type = settingsFormat.type;
      example =
        lib.literalExpression ''
        '';
      description = ''
        Spotify-player configuration.
        For available settings, see <>.
      '';
    };
    keymap = lib.mkOption {
      type = settingsFormat.type;
      example =
        lib.literalExpression ''
        '';
      description = ''
        Spotify-player configuration.
        For available settings, see <>.
      '';
      default = {
        actions = [
          {
            action = "AddToLiked";
            key_sequence = "C-l";
          }
          {
            action = "AddToPlaylist";
            key_sequence = "C-p";
          }
          {
            action = "AddToQueue";
            key_sequence = "C-w";
          }
          {
            action = "DeleteFromLiked";
            key_sequence = "C-L";
          }
        ];
        keymaps = [
          {
            command = "NextTrack";
            key_sequence = "J";
          }
          {
            command = "PreviousTrack";
            key_sequence = "K";
          }
          {
            command = "ResumePause";
            key_sequence = "space";
          }
          {
            command = "Repeat";
            key_sequence = "C-r";
          }
          {
            command = "Shuffle";
            key_sequence = "C-s";
          }
          {
            command = "SeekForward";
            key_sequence = ">";
          }
          {
            command = "SeekBackward";
            key_sequence = "<";
          }
          {
            command = "Quit";
            key_sequence = "Q";
          }
          {
            command = "ClosePopup";
            key_sequence = "esc";
          }
          {
            command = "SelectNextOrScrollDown";
            key_sequence = "j";
          }
          {
            command = "SelectPreviousOrScrollUp";
            key_sequence = "k";
          }
          {
            command = "SelectFirstOrScrollToTop";
            key_sequence = "g g";
          }
          {
            command = "SelectLastOrScrollToBottom";
            key_sequence = "G";
          }
          {
            command = "ChooseSelected";
            key_sequence = "enter";
          }
          {
            command = "RefreshPlayback";
            key_sequence = "g r";
          }
          {
            command = "ShowActionsOnSelectedItem";
            key_sequence = "a s";
          }
          {
            command = "ShowActionsOnCurrentTrack";
            key_sequence = "a c";
          }
          {
            command = "Search";
            key_sequence = "/";
          }
          {
            command = "BrowseUserPlaylists";
            key_sequence = "f p";
          }
          {
            command = "BrowseUserFollowedArtists";
            key_sequence = "f a";
          }
          {
            command = "BrowseUserSavedAlbums";
            key_sequence = "f b";
          }
          {
            command = "CurrentlyPlayingContextPage";
            key_sequence = "f c";
          }
          {
            command = "LyricsPage";
            key_sequence = "g v";
          }
          {
            command = "LibraryPage";
            key_sequence = "g l";
          }
          {
            command = "SearchPage";
            key_sequence = "g s";
          }
          {
            command = "BrowsePage";
            key_sequence = "g b";
          }
          {
            command = "Queue";
            key_sequence = "g q";
          }
          {
            command = "OpenCommandHelp";
            key_sequence = "?";
          }
          {
            command = "MovePlaylistItemUp";
            key_sequence = "C-k";
          }
          {
            command = "MovePlaylistItemDown";
            key_sequence = "C-j";
          }
          {
            command = "CreatePlaylist";
            key_sequence = "N";
          }
        ];
      };
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
    home.packages = [cfg.package];
    xdg.configFile."spotify-player/app.toml".source = settingsFormat.generate "app.toml" cfg.settings;
    # xdg.configFile."spotify-player/theme.toml".source = settingsFormat.generate "theme.toml" cfg.theme;
    xdg.configFile."spotify-player/keymap.toml".source = settingsFormat.generate "keymap.toml" cfg.keymap;
  };
}
