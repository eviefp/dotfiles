{ dotfiles, pkgs, config, lib, ... }:
let
  cfg = config.evie.wayland.waybar;
in
{
  options.evie.wayland.waybar = {
    enable = lib.mkEnableOption "waybar defaults";
    modules = {
      enableTV = lib.mkEnableOption "enable TV indicator";
      enableBT = lib.mkEnableOption "enable Bluetooth indicator";
      enableEmails = lib.mkEnableOption "enable emails indicator";
      enableCalendar = lib.mkOption {
        default = true;
        example = false;
        description = "enable calendars indicator";
        type = lib.types.bool;
      };
    };

    hyprland = {
      onlyActiveWorkspaces = lib.mkEnableOption "only show active workspaces";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.socat
      pkgs.wttrbar
    ];

    home.file.".config/waybar/scripts" = {
      recursive = true;
      source = ./../../../config/eww/scripts;
    };

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
          output = "DP-2";
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
            "custom/empty"
          ];

          modules-center = [
            "group/hardware"
            "group/media"
          ] ++ (if cfg.modules.enableEmails then [ "group/email" ] else [ ]);

          modules-right =
            (if cfg.modules.enableCalendar then [ "custom/events" ] else [ ])
            ++ [
              "custom/weather"
              "clock"
            ];

          "custom/separator" = {
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
            exec = "~/.config/waybar/scripts/get-active-window.sh";
            restart-interval = "30";
            format = "{}";
          };

          "custom/empty" = {
            format = "";
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
          };

          memory = {
            format = " {}%";
            on-click = "kitty -e btm";
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
            exec = "notmuch count tag:important";
            interval = 5;
            format = " {}";
          };

          "custom/email-unread" = {
            exec = "notmuch count tag:unread";
            interval = 5;
            format = " {}";
          };

          "custom/notifications" = {
            exec = "swaync-client -swb";
            on-click = "swaync-client -t -sw";
            return-type = "json";
            format = "{icon}";
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
            exec = "~/.config/waybar/scripts/waybar-khal.py";
            return-type = "json";
          };

          "group/media" = {
            orientation = "horizontal";
            modules =
              [ "wireplumber" ]
              ++ (if cfg.modules.enableTV then [ "custom/separator" "custom/tv" ] else [ ])
              ++ (if cfg.modules.enableBT then [ "custom/separator" "bluetooth" ] else [ ]);
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
          };

          "custom/tv" = {
            exec = "~/.config/waybar/scripts/tv-status.nu";
            interval = 1;
            format = "{icon} ";
            return-type = "json";
            format-icons = {
              # nope, need to do json and this needs to be the alt
              on = "<span foreground='green'></span>";
              off = "";
            };
            on-click = "~/.config/waybar/scripts/toggle-tv.sh";
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

          clock = {
            format = "{:%a, %d %b %T}";
            tooltip-format = "{calendar}";
            interval = 1;
            timezones = [ "Europe/Bucharest" "Europe/Berlin" "America/New_York" "America/Los_Angeles" ];
            calendar = {
              mode = "year";
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

        tooltip {
          opacity: 1;
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
        #clock,
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
        #clock,
        #tray {
          padding: 0 0.5rem;
        }

        /* all but rightmost item in a group */
        #cpu,
        #memory,
        #custom-email-important,
        #custom-email-unread,
        #custom-notifications,
        #wireplumber,
        #custom-tv {
          /* padding-right: 0.5rem; */
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

        label:focus {
          background-color: @activeBg;
        }

      '';
    };
  };
}
