{ lib, config, pkgs, hyprland, hyprpaper, hyprpicker, ... }:
let
  cfg = config.evie.wayland;
  mkMonitor = mon: "monitor=${mon.name},${mon.resolution},${mon.position},1";
  monitors = lib.lists.foldr (monitor: conf: "${conf}\n${mkMonitor monitor}") "" cfg.monitors;
  mkMonitorBind = mon: "bind = $mainMod, ${mon.keybind}, focusmonitor, ${mon.name}";
  monitor-binds = lib.lists.foldr (monitor: conf: "${conf}\n${mkMonitorBind monitor}") "" cfg.monitors;
  hyprland-package = hyprland.packages.${pkgs.system}.hyprland;
in
{
  imports = [
  ];

  config = lib.mkIf cfg.enable {

    home.packages = [
      pkgs.libsForQt5.qtwayland
      pkgs.libsForQt5.qt5ct
      pkgs.qt6.qtwayland
      pkgs.qt6Packages.qt6ct
      pkgs.libva
      pkgs.xdg-desktop-portal-hyprland
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
      extraConfig = ''
        # See https://wiki.hyprland.org/Configuring/Monitors/
        ${monitors}

        # notifications, wallpaper, status bar
        exec-once = swaync
        exec-once = hyprpaper
        exec-once = hypridle
        exec-once = eww d & eww open statusbar
        exec-once = ect --server

        # clipboard history
        exec-once = wl-paste --type text --watch cliphist store #Stores only text data
        exec-once = wl-paste --type image --watch cliphist store #Stores only image data

        # Source a file (multi-file configs)
        # source = ~/.config/hypr/myColors.conf

        # Some default env vars.
        env = XCURSOR_SIZE,24
        env = QT_QPA_PLATFORMTHEME,qt6ct # change to qt6ct if you have that
        env = NIXOS_OZONE_WL,hyprland

        # For all categories, see https://wiki.hyprland.org/Configuring/Variables/
        input {
            kb_layout = us
            kb_variant =
            kb_model =
            kb_options = compose:pause
            kb_rules =

            follow_mouse = 2

            touchpad {
                natural_scroll = no
            }

            sensitivity = 0 # -1.0 - 1.0, 0 means no modification.
        }

        general {
            # See https://wiki.hyprland.org/Configuring/Variables/ for more

            gaps_in = 5
            gaps_out = 20
            border_size = 2
            col.active_border = rgba(33ccffee) rgba(00ff99ee) 45deg
            col.inactive_border = rgba(595959aa)

            layout = master
            no_cursor_warps = true

            resize_on_border = true
            hover_icon_on_border = true

            # Please see https://wiki.hyprland.org/Configuring/Tearing/ before you turn this on
            allow_tearing = false

            cursor_inactive_timeout = 0
        }

        decoration {
            # See https://wiki.hyprland.org/Configuring/Variables/ for more

            rounding = 10
            active_opacity = 0.9
            inactive_opacity = 0.75
            fullscreen_opacity = 1


            blur {
                enabled = true
                size = 3
                passes = 1
            }

            drop_shadow = yes
            shadow_range = 8
            shadow_render_power = 2
            col.shadow = rgba(1a1a1aee)
        }

        animations {
            enabled = yes

            # Some default animations, see https://wiki.hyprland.org/Configuring/Animations/ for more

            bezier = myBezier, 0.05, 0.9, 0.1, 1.05

            animation = windows, 1, 7, myBezier
            animation = windowsOut, 1, 7, default, popin 80%
            animation = border, 1, 10, default
            animation = borderangle, 1, 8, default
            animation = fade, 1, 7, default
            animation = workspaces, 1, 6, default
        }

        group {
          groupbar {
            # enabled = false;
          }
        }

        dwindle {
            # See https://wiki.hyprland.org/Configuring/Dwindle-Layout/ for more
            pseudotile = yes # master switch for pseudotiling. Enabling is bound to mainMod + P in the keybinds section below
            preserve_split = yes # you probably want this
        }

        master {
            # See https://wiki.hyprland.org/Configuring/Master-Layout/ for more
            new_is_master = false
        }

        gestures {
            # See https://wiki.hyprland.org/Configuring/Variables/ for more
            workspace_swipe = off
        }

        misc {
            # See https://wiki.hyprland.org/Configuring/Variables/ for more
            force_default_wallpaper = -1 # Set to 0 to disable the anime mascot wallpapers
            key_press_enables_dpms = true
        }

        plugin {
        }

        # Example windowrule v1
        # windowrule = float, ^(kitty)$
        # Example windowrule v2
        # windowrulev2 = float,class:^(kitty)$,title:^(kitty)$
        # See https://wiki.hyprland.org/Configuring/Window-Rules/ for more
        windowrulev2 = forcergbx,class:(qutebrowser)
        windowrulev2 = opacity 1.0 override 0.7,class:(Emacs)
        windowrulev2 = opacity 1.0 override 0.7,class:(kitty)
        windowrulev2 = workspace 1,class:(discord)
        windowrulev2 = workspace 1,class:(Signal)

        # Set programs that you use
        $terminal = kitty
        $menu = rofi -show drun
        $pass = tessen -p pass -d rofi -a autotype
        $screenshot = grimblast copy area
        $cliphist = cliphist list | rofi -dmenu | cliphist decode | wl-copy
        $notifications = swaync-client -t -sw
        $sleep = sleep 1s; hyprctl dispatch dpms off

        # See https://wiki.hyprland.org/Configuring/Keywords/ for more
        $mainMod = SUPER
        $shiftMod = SUPER_SHIFT

        # Example binds, see https://wiki.hyprland.org/Configuring/Binds/ for more
        bind = $shiftMod, Return, exec, $terminal
        bind = $shiftMod, Q, exit,
        bind = $shiftMod, O, toggleOpaque,
        bind = $shiftMod, I, exec, hyprpicker --format=hex --no-fancy --autocopy
        bind = $shiftMod, P, exec, $sleep
        bind = $shiftMod, T, exec, /home/evie/.config/eww/scripts/toggle-tv.sh

        bind = $shiftMod, C, killactive,
        bind = $mainMod, M, exec, $screenshot
        bind = $mainMod, V, exec, $cliphist
        bind = $mainMod, F, togglefloating,
        bind = $mainMod, P, exec, $menu
        bind = $mainMod, O, exec, $pass
        bind = $mainMod, N, exec, $notifications
        bind = $mainMod, Return, layoutmsg, swapwithmaster
        bind = $mainMod, G, fullscreen, 0

        # Move focus with mainMod + arrow keys
        bind = $mainMod, J, layoutmsg, cycleprev
        bind = $mainMod, K, layoutmsg, cyclenext
        bind = $mainMod, backslash, layoutmsg, addmaster
        bind = $mainMod, apostrophe, layoutmsg, removemaster

        ${monitor-binds}

        binde = $mainMod, H, resizeactive, -10 0
        binde = $mainMod, L, resizeactive, 10 0

        # Switch workspaces with mainMod + [0-9]
        # bind = $mainMod, 1, focusworkspaceoncurrentmonitor, 1
        bind = $mainMod, 1, focusworkspaceoncurrentmonitor, 1
        bind = $mainMod, 2, focusworkspaceoncurrentmonitor, 2
        bind = $mainMod, 3, focusworkspaceoncurrentmonitor, 3
        bind = $mainMod, 4, focusworkspaceoncurrentmonitor, 4
        bind = $mainMod, 5, focusworkspaceoncurrentmonitor, 5
        bind = $mainMod, 6, focusworkspaceoncurrentmonitor, 6
        bind = $mainMod, 7, focusworkspaceoncurrentmonitor, 7
        bind = $mainMod, 8, focusworkspaceoncurrentmonitor, 8
        bind = $mainMod, 9, focusworkspaceoncurrentmonitor, 9
        bind = $mainMod, 0, focusworkspaceoncurrentmonitor, 10

        # Move active window to a workspace with mainMod + SHIFT + [0-9]
        bind = $mainMod SHIFT, 1, movetoworkspacesilent, 1
        bind = $mainMod SHIFT, 2, movetoworkspacesilent, 2
        bind = $mainMod SHIFT, 3, movetoworkspacesilent, 3
        bind = $mainMod SHIFT, 4, movetoworkspacesilent, 4
        bind = $mainMod SHIFT, 5, movetoworkspacesilent, 5
        bind = $mainMod SHIFT, 6, movetoworkspacesilent, 6
        bind = $mainMod SHIFT, 7, movetoworkspacesilent, 7
        bind = $mainMod SHIFT, 8, movetoworkspacesilent, 8
        bind = $mainMod SHIFT, 9, movetoworkspacesilent, 9
        bind = $mainMod SHIFT, 0, movetoworkspacesilent, 10

        # Example special workspace (scratchpad)
        bind = $mainMod, I, togglespecialworkspace, magic
        bind = $shiftMod, I, movetoworkspace, special:magic

        # Move/resize windows with mainMod + LMB/RMB and dragging
        bindm = $mainMod, mouse:272, movewindow
        bindm = $mainMod, mouse:273, resizewindow

        bind = $shiftMod, a, togglegroup,
        bind = $shiftMod, s, lockactivegroup, toggle
        bind = $shiftMod, h, moveintogroup, l
        bind = $shiftMod, l, moveintogroup, r
        bind = $shiftMod, j, moveintogroup, d
        bind = $shiftMod, k, moveintogroup, u
        bind = $shiftMod, m, moveoutofgroup,
        bind = $mainMod, a, changegroupactive, b
        bind = $mainMod, s, changegroupactive, f

        bind = $shiftMod, w, submap, eww
        submap = eww
        binde = ctrl,j,exec,~/.config/eww/scripts/down.sh
        binde = ctrl,k,exec,~/.config/eww/scripts/up.sh
        bind  = ctrl,Return,exec,~/.config/eww/scripts/select.sh
        bind  = ctrl,Return,submap,reset
        bind  = ,escape,exec,eww close cpu
        bind  = ,escape,submap,reset
        submap = reset
      '';
    };
  };
}
