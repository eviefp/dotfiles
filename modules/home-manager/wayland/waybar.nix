{ dotfiles, pkgs, config, lib, ... }:
let
  cfg = config.evie.wayland.waybar;
in
{
  options.evie.wayland.waybar = {
    enable = lib.mkEnableOption "waybar defaults";

    outputMonitor = lib.mkOption {
      type = lib.types.str;
      example = "DP-1";
    };


    modules = {
      enableTV = lib.mkEnableOption "enable TV indicator";
      enableBT = lib.mkEnableOption "enable Bluetooth indicator";
      enableWebcam = lib.mkEnableOption "enable webcam indicator";
      enableEmails = lib.mkEnableOption "enable emails indicator";
      enableCalendar = lib.mkEnableOption "enable calendars indicator";
    };

    hyprland = {
      onlyActiveWorkspaces = lib.mkOption {
        default = true;
        example = false;
        description = "only show active workspaces";
        type = lib.types.bool;
      };

    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.wttrbar
    ];

    programs.waybar = {
      enable = true;
      package = dotfiles.self.packages.${pkgs.system}.waybar;
      settings = {
        systemd = {
          enable = true;
          target = "hyprland-session.target";
        };

        mainBar = {
          layer = "top";
          output = cfg.outputMonitor;
          position = "bottom";
          spacing = 8;
          margin-top = 2;
          margin-bottom = 2;
          margin-left = 8;
          margin-right = 8;
          mode = "dock";

          reload_on_style_change = true;

          modules-left = [
            "custom/appmenu"
            "hyprland/workspaces"
            "custom/window"
          ];

          modules-center = [
            "group/hardware"
            "group/media"
          ] ++ (if cfg.modules.enableEmails then [ "group/email" ] else [ ]);

          modules-right =
            (if cfg.modules.enableCalendar then [ "custom/events" ] else [ ])
            ++ [
              "custom/weather"
              "group/date-time"
            ];

          "custom/separator" = {
            tooltip = false;
            format = "❱";
          };

          ## modules
          "custom/appmenu" = {
            format = "λ";
            tooltip-format = "Open application launcher.";
            on-click = "rofi -show drun";
            tooltip = false;
          };

          "hyprland/workspaces" = {
            active-only = cfg.hyprland.onlyActiveWorkspaces;
            all-outputs = true;
            format = "{id}";
            on-scroll-up = "hyprctl dispatch workspace r-1";
            on-scroll-down = "hyprctl dispatch workspace r+1";
            on-click = "activate";
            sort-by = "output";
          };

          "custom/window" = {
            exec = "${lib.getExe dotfiles.self.packages.${pkgs.system}.scripts.get-active-window}";
            max-length = 32;
            restart-interval = "30";
            format = "{}";
            tooltip = false;
          };

          "group/hardware" = {
            orientation = "horizontal";
            modules = [
              "systemd-failed-units"
              "custom/separator"
              "cpu"
              "custom/separator"
              "memory"
            ];
          };

          cpu = {
            format = " {usage}%";
            on-click = "kitty -e btm";
            states = {
              low = 20;
              medium = 40;
              high = 80;
            };
          };

          memory = {
            format = " {}%";
            on-click = "kitty -e btm";
            states = {
              low = 20;
              medium = 40;
              high = 80;
            };
          };

          systemd-failed-units = {
            hide-on-ok = false;
            format = "✗ {nr_failed}";
            format-ok = "✓";
            system = true;
            user = true;
          };

          "group/email" = {
            orientation = "horizontal";
            modules = [
              "custom/email-important"
              "custom/separator"
              "custom/email-unread"
              "custom/separator"
              "custom/notifications"
            ];
          };

          "custom/email-important" = {
            exec = "${lib.getExe dotfiles.self.packages.${pkgs.system}.scripts.email-status} important";
            format = " {}";
            interval = 5;
            return-type = "json";
            tooltip = true;
          };

          "custom/email-unread" = {
            exec = "${lib.getExe dotfiles.self.packages.${pkgs.system}.scripts.email-status} unread";
            format = " {}";
            interval = 5;
            return-type = "json";
            tooltip = true;
          };

          "custom/notifications" = {
            exec = "swaync-client -swb";
            on-click = "swaync-client -t -sw";
            return-type = "json";
            format = "{icon} {}";
            min-length = 3;
            format-icons = {
              notification = "<span foreground='red'><sup></sup></span>";
              none = "";
              dnd-notification = "<span foreground='red'><sup></sup></span>";
              dnd-none = "";
              inhibited-notification = "<span foreground='red'><sup></sup></span>";
              inhibited-none = "";
              dnd-inhibited-notification = "<span foreground='red'><sup></sup></span>";
              dnd-inhibited-none = "";
            };
          };

          "custom/events" = {
            format = "{}";
            tooltip = true;
            interval = 300;
            format-icons = {
              default = "";
            };
            exec = "${lib.getExe dotfiles.self.packages.${pkgs.system}.scripts.waybar-khal}";
            return-type = "json";
            max-length = 32;
          };

          "group/media" = {
            orientation = "horizontal";
            modules =
              [ "wireplumber" ]
              ++ (if cfg.modules.enableTV then [ "custom/separator" "custom/tv" ] else [ ])
              ++ (if cfg.modules.enableBT then [ "custom/separator" "bluetooth" ] else [ ])
              ++ (if cfg.modules.enableWebcam then [ "custom/separator" "custom/webcam" ] else [ ]);
          };

          wireplumber = {
            format = "{icon} {volume}% {format_source}";
            format-muted = " {format_source}";
            format-source = " {}";
            format-source-muted = " ";
            format-icons = {
              default = [ "" "" ];
            };
            tooltip-format = "{node_name} {source_desc}";
            on-click = "pactl set-sink-mute @DEFAULT_SINK@ toggle";
            on-click-right = "pactl set-source-mute @DEFAULT_SOURCE@ toggle";
            states = {
              low = 0;
              medium = 30;
              high = 50;
            };
          };

          "custom/tv" = {
            exec = "${lib.getExe dotfiles.self.packages.${pkgs.system}.scripts.tv-status}";
            interval = 1;
            format = "{icon} ";
            return-type = "json";
            tooltip = false;
            format-icons = {
              # nope, need to do json and this needs to be the alt
              on = "<span foreground='green'></span>";
              off = "";
            };
            on-click = "${lib.getExe dotfiles.self.packages.${pkgs.system}.scripts.tv-toggle}";
          };

          bluetooth = {
            format = " {status}";
            format-connected = " {device_alias}";
            format-connected-battery = " {device_battery_percentage}% {device_alias}";
            tooltip-format = "{controller_alias}\t{controller_address}\n\n{num_connections} connected";
            tooltip-format-connected = "{controller_alias}\t{controller_address}\n\n{num_connections} connected\n\n{device_enumerate}";
            tooltip-format-enumerate-connected = "{device_alias}\t{device_address}";
            tooltip-format-enumerate-connected-battery = "{device_alias}\t{device_address}\t{device_battery_percentage}%";
            max-length = 10;
          };

          "custom/webcam" = {
            format = "{} ";
            tooltip = true;
            interval = 10;
            exec = "${lib.getExe dotfiles.self.packages.${pkgs.system}.scripts.webcam-status}";
            return-type = "json";
          };

          tray = {
            icon-size = 21;
            spacing = 10;
          };

          "custom/weather" = {
            format = "{}°C";
            tooltip = true;
            interval = 3600;
            exec = "wttrbar --location bucharest --date-format \"%d/%m\" --lang en --observation-time";
            return-type = "json";
          };

          "group/date-time" = {
            orientation = "horizontal";
            modules = [
              "clock#date"
              "custom/separator"
              "clock"
            ];
          };

          clock = {
            format = "{:%T %Z}"; # todo: add timezone
            interval = 1;
            timezones = [ "Europe/Bucharest" "Europe/Berlin" "America/New_York" "America/Los_Angeles" ];
            tooltip = false;
            actions = {
              on-scroll-up = "tz_up";
              on-scroll-down = "tz_down";
            };
          };

          "clock#date" = {
            format = "{:%a, %d %b}";
            smooth-scrolling-treshold = 4;
            tooltip = true;
            tooltip-format = "<tt>{calendar}</tt>";
            calendar = {
              mode = "year";
              mode-mon-col = 3;
              on-scroll = 1;
              on-click-right = "mode";
              format = {
                months = "<span color='#ffead3'><b>{}</b></span>";
                days = "<span color='#ecc6d9'><b>{}</b></span>";
                weeks = "<span color='#99ffdd'><b>W{}</b></span>";
                weekdays = "<span color='#ffcc66'><b>{}</b></span>";
                today = "<span color='#ff6699'><b><u>{}</u></b></span>";
              };
            };
            actions = {
              on-right-click = "mode";
              on-scroll-up = "shift_up";
              on-scroll-down = "shift_down";
            };
          };

        };
      };
      # can also be file
      style = ''
        @define-color rosewater #f5e0dc;
        @define-color flamingo #f2cdcd;
        @define-color pink #f5c2e7;
        @define-color mauve #cba6f7;
        @define-color red #f38ba8;
        @define-color maroon #eba0ac;
        @define-color peach #fab387;
        @define-color yellow #f9e2af;
        @define-color green #a6e3a1;
        @define-color teal #94e2d5;
        @define-color sky #89dceb;
        @define-color sapphire #74c7ec;
        @define-color blue #89b4fa;
        @define-color lavender #b4befe;

        @define-color base alpha(#000000, 0.1);
        /* @define-color surface alpha(@lavender, 0.1); */
        @define-color surface alpha(#000000, 0.7);
        @define-color text #cdd6f4;
        @define-color hoverBg alpha(@mauve, 1);
        @define-color hoverFg #000000;
        @define-color activeBg alpha(@mauve, 1);
        @define-color activeFg #000000;
        @define-color shadow alpha(@blue, 0.7);

        * {
          all: unset;
          font-size: 1rem;
          font-weight: 900;
          color: @pink;
          font-family:
            JetBrainsMonoNerdFont,
            Font Awesome,
            Roboto,
            Helvetica,
            Arial,
            sans-serif;
          background-clip: border-box;
        }

        button {
          min-width: 32px;
          min-height: 32px;
          border-radius: 999px;
          color: @text;
          background-color: @surface;
          box-shadow: 0px 0px 8px 4px @shadow inset;
          transition:
            all 50ms cubic-bezier(0.55, 0, 0.28, 1.682),
            box-shadow 50ms ease-in-out,
            background-color 200ms ease-in-out;
        }

        button:hover {
          color: @hoverFg;
          background: @hoverBg;
        }

        button.active {
          color: @activeFg;
          background: @activeBg;
        }

        #custom-window {
          background-color: transparent;
        }

        #custom-window {
          margin-right: 8px;
        }

        .blue,
        #custom-separator {
          color: @blue;
          padding: 0 0.3rem;
        }

        /* all groups here except workspaces */
        #custom-appmenu,
        #custom-window,
        #hardware,
        #email,
        #media,
        #custom-events,
        #custom-weather,
        #date-time,
        #tray {
          background-color: @surface;
          border-radius: 999px;
          box-shadow: 0px 0px 8px 4px @shadow inset;
        }

        /* all groups also here, except appmenu and workspaces */
        #custom-window,
        #hardware,
        #email,
        #media,
        #custom-events,
        #custom-weather,
        #date-time,
        #tray {
          padding: 0 0.5rem;
        }

        #custom-appmenu {
          font-size: 24px;
          min-width: 32px;
          min-height: 32px;
          padding-right: 4px;
          border-radius: 999px;
        }

        #workspaces button {
          margin-right: 2px;
        }
        #workspaces button:last-child {
          margin-right: 0;
        }

        @keyframes blink {
          to {
            background-color: @activeBg;
          }
        }

        #battery.critical:not(.charging) {
          background-color: @activeBg;
          animation-name: blink;
          animation-duration: 0.5s;
          animation-timing-function: linear;
          animation-iteration-count: infinite;
          animation-direction: alternate;
        }

        #tray > .passive {
          -gtk-icon-effect: dim;
        }

        #tray > .needs-attention {
          -gtk-icon-effect: highlight;
          background-color: @activeBg;
        }

        #systemd-failed-units.ok {
          color: @green;
        }

        #systemd-failed-units.degraded {
          color: @red;
        }

        #cpu.low, #memory.low {
          color: @green;
        }
        #cpu.medium, #memory.medium, #wireplumber.medium, #custom-email-unread.unread {
          color: @yellow;
        }
        #cpu.high, #memory.high, #wireplumber.low, #custom-email-important.unread, #custom-notifications.notification {
          color: @red;
        }

        /* #custom-notifications.dnd-none, #custom-notifications.dnd-notification */

        label:focus {
          background-color: @activeBg;
        }

        tooltip {
          background-color: @shadow;
          border-radius: 12px;
          border: 1px solid @mauve;
          padding: 6px;
        }

        #custom-webcam.active {
          color: @red;
        }
        #custom-webcam.enabled {
          /* color: @green; */
        }
        #custom-webcam.none {
          color: @yellow;
        }
      '';
    };
  };
}
