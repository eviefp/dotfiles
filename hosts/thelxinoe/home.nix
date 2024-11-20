{ dotfiles, config, ... }:
{
  imports = with dotfiles.self.homeManagerModules; [
    calendar
    common
    email
    gpg
    gui
    programs.dev.default
    programs.streaming
    programs.spotify-player
    sops
    wayland.default
  ];

  config = {

    evie = {
      programs.spotify = {
        enable = true;
        settings = {
          client_id_command = "cat ${config.sops.secrets.spotifyAppClientId.path}";
        };
        keymap = {
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
              command = "VolumeUp";
              key_sequence = "+";
            }
            {
              command = "VolumeDown";
              key_sequence = "-";
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
              command = "BrowseFollowedArtists";
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
              command = "LyricPage";
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
      programs.editors.emacs.locals = {
        enable = true;
        file = ./thelxinoe.el;
      };

      wayland = {
        eww-monitor = "2";
        showTV = true;
        useSshMailCalendar = false;
        showMail = true;
        showCalendar = true;
        monitors = [
          {
            name = "DP-3";
            resolution = "1920x1080@239.76";
            position = "0x0";
            keybind = "W";
          }
          {
            name = "DP-1";
            resolution = "1920x1080@239.76";
            position = "1920x0";
            keybind = "E";
          }
          {
            name = "DP-2";
            resolution = "1920x1080@239.76";
            position = "3840x0";
            keybind = "R";
          }
          {
            name = "HDMI-A-1";
            resolution = "1920x1080@60";
            position = "5760x0";
            keybind = "T";
          }
        ];
      };
    };
  };
}
