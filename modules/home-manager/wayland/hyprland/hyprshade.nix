{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.evie.wayland.hyprshade;
in {
  options.evie.wayland.hyprshade = {
    enable = lib.mkEnableOption "hyprshade defaults";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [pkgs.hyprshade];

    home.file.".config/hypr/shaders" = {
      recursive = true;
      source = ./shaders;
    };

    # xdg.configFile."hyprshade/config.toml".text = ''
    #   [[shades]]
    #   name = "blue-light-filter-modified"
    #   start_time = 19:00:00
    #   end_time = 07:00:00
    # '';

    # systemd.user.services.hyprshade = {
    #   Unit = {
    #     Description = "Apply screen filter";
    #   };
    #   Service = {
    #     Type = "oneshot";
    #     ExecStart = "${pkgs.hyprshade}/bin/hyprshade auto";
    #   };
    # };
    #
    # systemd.user.timers.hyprshade = {
    #   Unit = {
    #     Description = "Run hyprshade auto every minute";
    #   };
    #   Timer = {
    #     OnBootSec = "1min";
    #     OnUnitActiveSec = "1min";
    #     Unit = "hyprshade.service";
    #
    #     OnCalendar = [
    #       "*-*-* 19:00:00"
    #       "*-*-* 07:00:00"
    #     ];
    #   };
    #   Install = {
    #     WantedBy = [ "timers.target" ];
    #   };
    # };
  };
}
