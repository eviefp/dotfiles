{ lib, config, pkgs, hyprland, grab-workspace, hycov, hyprpaper, hyprpicker, ... }:
let
  cfg = config.evie.hyprland;
  hyprland-package = hyprland.packages.${pkgs.system}.hyprland;
  grab-workspace-package = pkgs.gcc13Stdenv.mkDerivation {
    pname = "grab-workspace";
    version = "0.1";
    src = grab-workspace;

    nativeBuildInputs = with pkgs; [ clang pkg-config ];

    buildPhase = ''
      make
    '';

    installPhase = ''
      mkdir -p $out/lib
      cp grab-workspace.so $out/lib/libgrab-workspace.so
    '';

    buildInputs = [ hyprland-package hyprland-package.buildInputs ];

    meta = with pkgs.lib; {
      homepage = "https://github.com/CMurtagh-LGTM/grab-workspace";
      description = "A plugin to grab a workspace and display it on the current monitor.";
      license = licenses.mit;
      platforms = platforms.linux;
    };
  };
in
{
  imports = [
  ];

  config = {

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
        grab-workspace-package
        hycov.packages.${pkgs.system}.hycov
      ];
      extraConfig = ''
        # See https://wiki.hyprland.org/Configuring/Monitors/
        monitor=DP-1,1920x1080@239.76,0x0,1
        monitor=DP-2,1920x1080@239.76,1920x0,1
        monitor=DP-3,1920x1080@239.76,3840x0,1
        monitor HDMI-A-2,1920x1080@60,5760x0,1

        # notifications, wallpaper, status bar
        exec-once = swaync
        exec-once = hyprpaper
        exec-once = eww d & eww open statusbar

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

            follow_mouse = 1

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

        # Example per-device config
        # See https://wiki.hyprland.org/Configuring/Keywords/#executing for more
        device:epic-mouse-v1 {
            sensitivity = -0.5
        }

        plugin {
            hycov {
                overview_gappo = 60 #gaps width from screen
                overview_gappi = 24 #gaps width from clients
                hotarea_size = 0 #hotarea size in bottom left,10x10
                enable_hotarea = 0 # enable mouse cursor hotarea
            }
        }

        # Example windowrule v1
        # windowrule = float, ^(kitty)$
        # Example windowrule v2
        # windowrulev2 = float,class:^(kitty)$,title:^(kitty)$
        # See https://wiki.hyprland.org/Configuring/Window-Rules/ for more
        windowrulev2 = nomaximizerequest, class:.* # You'll probably like this.

        windowrulev2 = forcergbx,class:(qutebrowser)
        windowrulev2 = opacity 1.0 override 0.7,class:(Emacs)
        windowrulev2 = opacity 1.0 override 0.7,class:(kitty)
        windowrulev2 = workspace 1,class:(discord)
        windowrulev2 = workspace 1,class:(Signal)

        # Set programs that you use
        $terminal = kitty
        $menu = rofi -show run
        $pass = tessen -p pass -d rofi -a autotype
        $screenshot = grim -g "$(slurp)" - | wl-copy
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

        bind = $mainMod, C, killactive,
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

        bind = $mainMod, W, focusmonitor, DP-1
        bind = $mainMod, E, focusmonitor, DP-2
        bind = $mainMod, R, focusmonitor, DP-3
        bind = $mainMod, T, focusmonitor, HDMI-A-2

        binde = $mainMod, H, resizeactive, -10 0
        binde = $mainMod, L, resizeactive, 10 0

        # Switch workspaces with mainMod + [0-9]
        bind = $mainMod, 1, grab-workspace, 1
        bind = $mainMod, 2, grab-workspace, 2
        bind = $mainMod, 3, grab-workspace, 3
        bind = $mainMod, 4, grab-workspace, 4
        bind = $mainMod, 5, grab-workspace, 5
        bind = $mainMod, 6, grab-workspace, 6
        bind = $mainMod, 7, grab-workspace, 7
        bind = $mainMod, 8, grab-workspace, 8
        bind = $mainMod, 9, grab-workspace, 9
        bind = $mainMod, 0, grab-workspace, 10

        # Move active window to a workspace with mainMod + SHIFT + [0-9]
        bind = $mainMod SHIFT, 1, movetoworkspace, 1
        bind = $mainMod SHIFT, 2, movetoworkspace, 2
        bind = $mainMod SHIFT, 3, movetoworkspace, 3
        bind = $mainMod SHIFT, 4, movetoworkspace, 4
        bind = $mainMod SHIFT, 5, movetoworkspace, 5
        bind = $mainMod SHIFT, 6, movetoworkspace, 6
        bind = $mainMod SHIFT, 7, movetoworkspace, 7
        bind = $mainMod SHIFT, 8, movetoworkspace, 8
        bind = $mainMod SHIFT, 9, movetoworkspace, 9
        bind = $mainMod SHIFT, 0, movetoworkspace, 10

        # Example special workspace (scratchpad)
        bind = $mainMod, S, togglespecialworkspace, magic
        bind = $shiftMod, S, movetoworkspace, special:magic

        # Scroll through existing workspaces with mainMod + scroll
        bind = $mainMod, mouse_down, grab-workspace, e+1
        bind = $mainMod, mouse_up, grab-workspace, e-1

        # Move/resize windows with mainMod + LMB/RMB and dragging
        bindm = $mainMod, mouse:272, movewindow
        bindm = $mainMod, mouse:273, resizewindow

        bind = $shiftMod, x, hycov:enteroverview
        bind = $shiftMod, c, hycov:leaveoverview
        bind = $shiftMod, z, hycov:toggleoverview

        bind = $shiftMod, h, hycov:movefocus,l
        bind = $shiftMod, l, hycov:movefocus,r
        bind = $shiftMod, j, hycov:movefocus,d
        bind = $shiftMod, k, hycov:movefocus,u
      '';
    };
  };
}
