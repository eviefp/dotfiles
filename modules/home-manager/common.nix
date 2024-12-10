/****************************************************************************
  * Common home-manager module
  *
  * Adds the modules/settings that all of my systems use.
  **************************************************************************/
{ dotfiles, lib, config, osConfig, ... }:
let
  cfg = config.evie.common;
  removeCommon = n: _: n != "common";
  notAttrs = n: !(lib.isAttrs n);
in
{
  options.evie.common = {
    enable = lib.mkEnableOption "common config";
  };

  imports = lib.collect notAttrs (lib.filterAttrs removeCommon dotfiles.self.homeManagerModules);

  config = lib.mkIf cfg.enable
    (lib.mkMerge [
      {
        evie = {
          dev = {
            nix.enable = true;
            tools.enable = true;
          };
          editors = {
            neovim.enable = true;
          };

          system = {
            fonts.enable = true;
            home-manager.enable = true;
          };

          term = {
            enable = true;
            experimental.enable = true;
            scripts.enable = true;
            text.enable = true;
            tui.enable = true;
          };
        };
      }
      (lib.mkIf config.evie.wayland.enable {
        evie = {
          programs = {
            chat.enable = true;
            gui.enable = true;
            qutebrowser.enable = true;
          };
          term = {
            kitty.enable = true;
            spotify = {
              enable = true;
              settings = {
                client_id_command = "cat ${osConfig.sops.secrets.spotifyAppClientId.path}";
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
          };

          wayland = lib.mkIf config.evie.wayland.hyprland.enable {
            eww.enable = true;
            rofi.enable = true;
            swaync.enable = true;

            hypridle.enable = true;
            hyprlock.enable = true;
            hyprpaper.enable = true;
          };
        };
      })
    ]
    );
}

