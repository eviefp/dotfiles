/**
**************************************************************************
* aerospace module
************************************************************************
*/
{
  lib,
  config,
  pkgs,
  ...
}: let
  cfg = config.evie.macos.aerospace;
in {
  options.evie.macos.aerospace = {
    enable = lib.mkEnableOption "aerospace defaults";
  };

  config = lib.mkIf cfg.enable {
    programs.aerospace = {
      enable = true;

      launchd = {
        enable = true;
        keepAlive = true;
      };

      userSettings = {
        exec.env-vars = {
          PATH = ''/etc/profiles/per-user/${config.home.username}/bin:''${PATH}'';
        };
        mode = {
          main.binding = {
            cmd-enter = "exec-and-forget open --new -a ${pkgs.kitty}/bin/kitty";
            cmd-space = "layout accordion tiles horizontal";
            cmd-f = "layout floating tiling";
            cmd-g = "fullscreen";

            # focus
            cmd-h = ["focus --boundaries all-monitors-outer-frame --boundaries-action wrap-around-all-monitors left" "move-mouse window-lazy-center"];
            cmd-j = ["focus --boundaries all-monitors-outer-frame --boundaries-action wrap-around-all-monitors down" "move-mouse window-lazy-center"];
            cmd-k = ["focus --boundaries all-monitors-outer-frame --boundaries-action wrap-around-all-monitors up" "move-mouse window-lazy-center"];
            cmd-l = ["focus --boundaries all-monitors-outer-frame --boundaries-action wrap-around-all-monitors right" "move-mouse window-lazy-center"];

            # Move window
            cmd-shift-h = "move left";
            cmd-shift-j = "move down";
            cmd-shift-k = "move up";
            cmd-shift-l = "move right";

            # Resize window
            cmd-ctrl-h = "resize width -50";
            cmd-ctrl-j = "resize height +50";
            cmd-ctrl-k = "resize height -50";
            cmd-ctrl-l = "resize width +50";

            # Workspaces
            cmd-1 = "workspace 1";
            cmd-2 = "workspace 2";
            cmd-3 = "workspace 3";
            cmd-4 = "workspace 4";
            cmd-5 = "workspace 5";
            cmd-6 = "workspace 6";
            cmd-7 = "workspace 7";
            cmd-8 = "workspace 8";
            cmd-9 = "workspace 9";
            cmd-0 = "workspace 10";

            cmd-shift-1 = "move-node-to-workspace --focus-follows-window 1";
            cmd-shift-2 = "move-node-to-workspace --focus-follows-window 2";
            cmd-shift-3 = "move-node-to-workspace --focus-follows-window 3";
            cmd-shift-4 = "move-node-to-workspace --focus-follows-window 4";
            cmd-shift-5 = "move-node-to-workspace --focus-follows-window 5";
            cmd-shift-6 = "move-node-to-workspace --focus-follows-window 6";
            cmd-shift-7 = "move-node-to-workspace --focus-follows-window 7";
            cmd-shift-8 = "move-node-to-workspace --focus-follows-window 8";
            cmd-shift-9 = "move-node-to-workspace --focus-follows-window 9";
            cmd-shift-0 = "move-node-to-workspace --focus-follows-window 10";
          };
        };
      };
    };
  };
}
