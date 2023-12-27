{ lib, config, pkgs, hyprland, grab-workspace, hyprpaper, ... }:
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
    home.file.".config/hypr/hyprpaper.conf".text = ''
      preload = ~/.config/wallpaper/1.jpg
      preload = ~/.config/wallpaper/2.jpg
      preload = ~/.config/wallpaper/3.jpg
      #if more than one preload is desired then continue to preload other backgrounds
      # preload = /path/to/next_image.png
      # .. more preloads

      #set the default wallpaper(s) seen on inital workspace(s) --depending on the number of monitors used
      wallpaper = DP-1,~/.config/wallpaper/1.jpg
      wallpaper = DP-2,~/.config/wallpaper/2.jpg
      wallpaper = DP-3,~/.config/wallpaper/3.jpg
      #if more than one monitor in use, can load a 2nd image
      # wallpaper = monitor2,/path/to/next_image.png
      # .. more monitors
    '';

    home.file.".config/wallpaper/1.jpg".source = ../../config/wallpapers/1.jpg;
    home.file.".config/wallpaper/2.jpg".source = ../../config/wallpapers/2.jpg;
    home.file.".config/wallpaper/3.jpg".source = ../../config/wallpapers/3.jpg;

    home.packages = [
      pkgs.libsForQt5.qtwayland
      pkgs.libsForQt5.qt5ct
      pkgs.qt6.qtwayland
      pkgs.libva
      pkgs.swaynotificationcenter
      pkgs.xdg-desktop-portal-hyprland
      pkgs.xdg-desktop-portal-gtk
      hyprpaper.packages.${pkgs.system}.hyprpaper
      pkgs.socat # needed by eww
    ];

    programs = {
      rofi = {
        enable = true;
        cycle = true;
        package = pkgs.rofi-wayland;
        pass = {
          enable = true;
        };
        # theme = "purple";
        theme = ./../../config/rofi-rounded-common.rasi;
        extraConfig = {
          show-icons = true;
          terminal = "kitty";
          sidebar-mode = true;

          kb-mode-next = "Control+l";
          kb-mode-previous = "Control+h";
          kb-row-up = "Control+k";
          kb-row-down = "Control+j";
          kb-remove-to-eol = "";
          kb-accept-entry = "Return";
          kb-mode-complete = "";
          kb-remove-char-back = "";
        };
      };

      # this is pretty janky on xmonad, also don't really need more UI
      eww = {
        enable = true;
        package = pkgs.eww-wayland;
        configDir = ./../../config/eww;
      };

    };

    wayland.windowManager.hyprland = {
      enable = true;
      package = hyprland-package;
      plugins = [
        grab-workspace-package
      ];
      extraConfig = ''
        # See https://wiki.hyprland.org/Configuring/Monitors/
        # monitor=,preferred,auto,auto
        monitor=DP-1,1920x1080@239.76,0x0,1
        monitor=DP-2,1920x1080@239.76,1920x0,1
        monitor=DP-3,1920x1080@239.76,3840x0,1
        monitor=HDMI-A-2,disable



        # See https://wiki.hyprland.org/Configuring/Keywords/ for more

        # Execute your favorite apps at launch
        # exec-once = waybar & hyprpaper & firefox
        exec-once = swaync & hyprpaper & eww d & eww open statusbar

        # Source a file (multi-file configs)
        # source = ~/.config/hypr/myColors.conf

        # Set programs that you use
        $terminal = kitty
        $fileManager = dolphin
        $menu = rofi -show run

        # Some default env vars.
        env = XCURSOR_SIZE,24
        env = QT_QPA_PLATFORMTHEME,qt5ct # change to qt6ct if you have that

        # For all categories, see https://wiki.hyprland.org/Configuring/Variables/
        input {
            kb_layout = us
            kb_variant =
            kb_model =
            kb_options =
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

            layout = dwindle

            # Please see https://wiki.hyprland.org/Configuring/Tearing/ before you turn this on
            allow_tearing = false
        }

        decoration {
            # See https://wiki.hyprland.org/Configuring/Variables/ for more

            rounding = 10

            blur {
                enabled = true
                size = 3
                passes = 1
            }

            drop_shadow = yes
            shadow_range = 4
            shadow_render_power = 3
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
            new_is_master = true
        }

        gestures {
            # See https://wiki.hyprland.org/Configuring/Variables/ for more
            workspace_swipe = off
        }

        misc {
            # See https://wiki.hyprland.org/Configuring/Variables/ for more
            force_default_wallpaper = -1 # Set to 0 to disable the anime mascot wallpapers
        }

        # Example per-device config
        # See https://wiki.hyprland.org/Configuring/Keywords/#executing for more
        device:epic-mouse-v1 {
            sensitivity = -0.5
        }

        # Example windowrule v1
        # windowrule = float, ^(kitty)$
        # Example windowrule v2
        # windowrulev2 = float,class:^(kitty)$,title:^(kitty)$
        # See https://wiki.hyprland.org/Configuring/Window-Rules/ for more
        windowrulev2 = nomaximizerequest, class:.* # You'll probably like this.


        # See https://wiki.hyprland.org/Configuring/Keywords/ for more
        $mainMod = SUPER

        # Example binds, see https://wiki.hyprland.org/Configuring/Binds/ for more
        bind = $mainMod, Return, exec, $terminal
        bind = $mainMod, C, killactive,
        bind = $mainMod, Q, exit,
        # bind = $mainMod, E, exec, $fileManager
        bind = $mainMod, F, togglefloating,
        bind = $mainMod, P, exec, $menu
        bind = $mainMod, X, pseudo, # dwindle
        bind = $mainMod, Space, togglesplit, # dwindle
        bind = $mainMod, G, fullscreen, 0

        # Move focus with mainMod + arrow keys
        bind = $mainMod, J, movefocus, l
        bind = $mainMod, K, movefocus, r

        bind = $mainMod, W, focusmonitor, DP-1
        bind = $mainMod, E, focusmonitor, DP-2
        bind = $mainMod, R, focusmonitor, DP-3
        bind = $mainMod, T, focusmonitor, HDMI-A-2

        bind = $mainMod, H, resizeactive, -20 0
        bind = $mainMod, L, resizeactive, 20 0

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
        bind = $mainMod SHIFT, S, movetoworkspace, special:magic

        # Scroll through existing workspaces with mainMod + scroll
        bind = $mainMod, mouse_down, workspace, e+1
        bind = $mainMod, mouse_up, workspace, e-1

        # Move/resize windows with mainMod + LMB/RMB and dragging
        bindm = $mainMod, mouse:272, movewindow
        bindm = $mainMod, mouse:273, resizewindow
      '';
    };
  };
}
