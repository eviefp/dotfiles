{ config, pkgs, hyprland, hyprpicker, ... }:
let
  cfg = config.evie.wayland;
  hyprland-package = hyprland.packages.${pkgs.system}.hyprland;
  switch-colors = pkgs.writeShellScriptBin "switch-colors" ''
    #!/usr/bin/env bash

    colorMode=`eww get colorMode`

    if [[ "$colorMode" == "light" ]]; then
      emacsclient --eval "(load-theme 'modus-vivendi-tritanopia :no-confirm)"
      ln -sf /home/evie/.config/kitty/dark.conf /home/evie/.config/kitty/theme.conf
      pkill -USR1 kitty
      eww update colorMode=dark
    else
      emacsclient --eval "(load-theme 'modus-operandi-tritanopia :no-confirm)"
      ln -sf /home/evie/.config/kitty/light.conf /home/evie/.config/kitty/theme.conf
      pkill -USR1 kitty
      eww update colorMode=light
    fi

  '';
in
{
  imports = [
  ];

  config = {

    home.packages = [
      pkgs.egl-wayland
      pkgs.libsForQt5.qtwayland
      pkgs.libsForQt5.qt5ct
      pkgs.qt6.qtwayland
      pkgs.qt6Packages.qt6ct
      pkgs.libva
      hyprland.packages.${pkgs.system}.xdg-desktop-portal-hyprland
      pkgs.xdg-desktop-portal-gtk
      hyprpicker.packages.${pkgs.system}.hyprpicker

      # clipboard history
      pkgs.cliphist

      # random stuff
      pkgs.wev
      pkgs.wtype
    ];

    wayland.windowManager.hyprland = {
      enable = true;

      package = hyprland-package;

      plugins = [
      ];

      systemd.variables = [ "--all" ];

      settings = {
        monitor =
          map (mon: "${mon.name}, ${mon.resolution}, ${mon.position}, 1") cfg.monitors
          ++ map (name: "${name}, disabled") cfg.disabledMonitors;

        exec-once = [
          "swaync"
          "hyprpaper"
          "hypridle"
          "wl-paste --type text --watch cliphist store" #Stores only text data
          "wl-paste --type image --watch cliphist store" #Stores only image data
          "blueman-applet"
          "hyprctl keyword monitor \"HDMI-A-2, disabled\"" # hacky, but eh
        ];

        env = [
          "XCURSOR_SIZE,24"
          "HYPRCURSOR_THEME,materialLight"
          "HYPRCURSOR_SIZE,24"
          "QT_QPA_PLATFORMTHEME,qt6ct" # change to qt6ct if you have that
          "NIXOS_OZONE_WL,hyprland"
        ];

        general = {
          border_size = 2;
          gaps_in = 5;
          gaps_out = 10;

          "col.active_border" = "rgba(33ccffee) rgba(00ff99ee) 45deg";
          "col.inactive_border" = "rgba(595959aa)";

          layout = "master";
          no_focus_fallback = true;
          resize_on_border = true;
          hover_icon_on_border = true;
          allow_tearing = false;
        };

        decoration = {
          rounding = 10;

          active_opacity = 0.9;
          inactive_opacity = 0.75;
          fullscreen_opacity = 1;

          drop_shadow = "yes";
          shadow_range = 8;
          shadow_render_power = 2;

          "col.shadow" = "rgba(1a1a1aee)";

          blur = {
            enabled = true;
            size = 5;
            passes = 3;
          };
        };

        animations = {
          enabled = "yes";
          bezier = "myBezier, 0.05, 0.9, 0.1, 1.05";
          animation = [
            "windows, 1, 7, myBezier"
            "windowsOut, 1, 7, default, popin 80%"
            "border, 1, 10, default"
            "borderangle, 1, 8, default"
            "fade, 1, 7, default"
            "workspaces, 1, 6, default"
          ];
        };

        input = {
          kb_layout = "us";
          kb_options = "compose:pause";

          follow_mouse = "0";

          touchpad = {
            natural_scroll = "no";
          };
          sensitivity = 0; # -1.0 - 1.0, 0 means no modification.
        };

        group = {
          groupbar = {
            font_size = 10;
            height = 16;
            render_titles = true;
            gradients = false;
            text_color = "rgba(c934f3ff)";
            "col.active" = "rgba(c934f3ff)";
            "col.inactive" = "rgba(707056ff)";
            "col.locked_active" = "rgba(c934f3ff)";
            "col.locked_inactive" = "rgba(707056ff)";
          };
        };

        misc = {
          disable_hyprland_logo = true;
          disable_splash_rendering = true;
          key_press_enables_dpms = true;
          new_window_takes_over_fullscreen = 2;
          initial_workspace_tracking = 1;
        };

        cursor = {
          inactive_timeout = 0;
          no_warps = true;
          enable_hyprcursor = true;
          # hide_on_key_press = true
        };

        debug = {
          disable_logs = false;
          disable_time = false;
          enable_stdout_logs = true;
        };

        dwindle = {
          pseudotile = "yes";
          preserve_split = "yes";
        };

        master = { };

        windowrulev2 = [
          "forcergbx,class:(qutebrowser)"
          "opacity 1.0 override 0.7,class:(kitty)"
          "opacity 1.0 override 0.7,class:(wezterm)"
          "workspace 1,class:(discord)"
          "workspace 1,class:(Signal)"
        ];

        plugin = { };

        "$terminal" = "kitty";
        "$menu" = "rofi -show drun";
        "$pass" = "tessen -p pass -d rofi -a autotype";
        "$screenshot" = "grimblast copy area";
        "$cliphist" = "cliphist list | rofi -dmenu | cliphist decode | wl-copy";
        "$notifications" = "swaync-client -t -sw";
        "$sleep" = "sleep 1s; hyprctl dispatch dpms off";
        "$toggleTimezones" = "eww open tz --toggle";
        "$mainMod" = "SUPER";
        "$shiftMod" = "SUPER_SHIFT";

        bind = [
          # Example binds, see https://wiki.hyprland.org/Configuring/Binds/ for more
          # "$shiftMod, Return, exec, [float;tile] wezterm start --always-new-process"
          "$shiftMod, Return, exec, $terminal"
          "$shiftMod, Q, exit,"
          "$shiftMod, O, toggleOpaque,"
          "$shiftMod, U, exec, hyprpicker --format=hex --no-fancy --autocopy"
          "$shiftMod, P, exec, $sleep"
          "$shiftMod, E, exec, /home/evie/.config/eww/scripts/toggle-tv.sh"
          "$shiftMod, T, exec, ${switch-colors}/bin/switch-colors"
          "$shiftMod, L, exec, hyprlock"

          "$shiftMod, C, killactive,"
          "$mainMod, M, exec, $screenshot"
          "$shiftMod, V, exec, $cliphist"
          "$mainMod, F, togglefloating,"
          "$mainMod, P, exec, $menu"
          "$mainMod, O, exec, $pass"
          "$mainMod, N, exec, $notifications"
          "$mainMod, Z, exec, $toggleTimezones"
          "$mainMod, C, exec, eww open cal --toggle"
          "$mainMod, V, exec, eww open events --toggle"
          "$mainMod, B, exec, eww open cpu --toggle"
          "$mainMod, Return, layoutmsg, swapwithmaster"
          "$mainMod, G, fullscreen, 0"

          # Move focus with mainMod + arrow keys
          "$mainMod, J, layoutmsg, cycleprev"
          "$mainMod, K, layoutmsg, cyclenext"
          "$mainMod, backslash, layoutmsg, addmaster"
          "$mainMod, apostrophe, layoutmsg, removemaster"

          # Switch workspaces with mainMod + [0-9]
          "$mainMod, 1, focusworkspaceoncurrentmonitor, 1"
          "$mainMod, 2, focusworkspaceoncurrentmonitor, 2"
          "$mainMod, 3, focusworkspaceoncurrentmonitor, 3"
          "$mainMod, 4, focusworkspaceoncurrentmonitor, 4"
          "$mainMod, 5, focusworkspaceoncurrentmonitor, 5"
          "$mainMod, 6, focusworkspaceoncurrentmonitor, 6"
          "$mainMod, 7, focusworkspaceoncurrentmonitor, 7"
          "$mainMod, 8, focusworkspaceoncurrentmonitor, 8"
          "$mainMod, 9, focusworkspaceoncurrentmonitor, 9"
          "$mainMod, 0, focusworkspaceoncurrentmonitor, 10"

          # Move active window to a workspace with mainMod + SHIFT + [0-9]
          "$mainMod SHIFT, 1, movetoworkspacesilent, 1"
          "$mainMod SHIFT, 2, movetoworkspacesilent, 2"
          "$mainMod SHIFT, 3, movetoworkspacesilent, 3"
          "$mainMod SHIFT, 4, movetoworkspacesilent, 4"
          "$mainMod SHIFT, 5, movetoworkspacesilent, 5"
          "$mainMod SHIFT, 6, movetoworkspacesilent, 6"
          "$mainMod SHIFT, 7, movetoworkspacesilent, 7"
          "$mainMod SHIFT, 8, movetoworkspacesilent, 8"
          "$mainMod SHIFT, 9, movetoworkspacesilent, 9"
          "$mainMod SHIFT, 0, movetoworkspacesilent, 10"

          # Example special workspace (scratchpad)
          "$mainMod, I, togglespecialworkspace, magic"
          "$shiftMod, I, movetoworkspace, special:magic"

          # Groups
          "$shiftMod, a, togglegroup,"
          "$shiftMod, s, lockactivegroup, toggle"
          "$shiftMod, h, moveintogroup, l"
          "$shiftMod, l, moveintogroup, r"
          "$shiftMod, j, moveintogroup, d"
          "$shiftMod, k, moveintogroup, u"
          "$shiftMod, m, moveoutofgroup,"
          "$mainMod, a, changegroupactive, b"
          "$mainMod, s, changegroupactive, f"
        ] ++ map (mon: "$mainMod, ${mon.keybind}, focusmonitor, ${mon.name}") cfg.monitors;

        binde = [
          "$mainMod, H, resizeactive, -10 0"
          "$mainMod, L, resizeactive, 10 0"
        ];

        bindm = [
          # Move/resize windows with mainMod + LMB/RMB and dragging
          "$mainMod, mouse:272, movewindow"
          "$mainMod, mouse:273, resizewindow"
        ];

        bindl = [
          ",switch:off:Lid Switch,exec,sleep 1s; eww o statusbar"
        ];
      };
    };

    home.pointerCursor = {
      gtk.enable = true;
      # x11.enable = true;
      package = pkgs.bibata-cursors;
      name = "Bibata-Modern-Classic";
      size = 16;
    };

    gtk = {
      enable = true;
      theme = {
        package = pkgs.flat-remix-gtk;
        name = "Flat-Remix-GTK-Grey-Darkest";
      };

      iconTheme = {
        package = pkgs.gnome.adwaita-icon-theme;
        name = "Adwaita";
      };

      font = {
        name = "Sans";
        size = 11;
      };
    };

  };
}
