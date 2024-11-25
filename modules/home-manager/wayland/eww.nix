{ lib, config, pkgs, ... }:
let
  cfg = config.evie.wayland.eww;
  wayland = config.evie.wayland;
  # TODO: these settings should probably be here
  sshThelxinoe = if wayland.useSshMailCalendar then "ssh thelxinoe " else "";
in
{
  options.evie.wayland.eww = {
    enable = lib.mkEnableOption "enable eww";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.socat
      pkgs.wlr-randr
      pkgs.alsa-utils
      pkgs.libnotify
    ];

    home.file.".config/eww-extras/mail-calendar-vars.yuck".text = ''
      (defpoll mail-important :initial "0" :interval "10s" "${sshThelxinoe}notmuch count tag:important")
      (defpoll mail-unread :initial "0" :interval "10s" "${sshThelxinoe}notmuch count tag:unread")
      (defpoll cal :initial `{ "title": "Loading...", "time": "00:00", "date": "2050-01-01" }` :interval "40s" "~/.config/eww/scripts/get-next-calendar-entry.nu single")
      (defpoll events :initial "[]" :interval "40s" "~/.config/eww/scripts/get-next-calendar-entry.nu all")
    '';

    home.file.".config/eww-extras/windows.yuck".text = ''
            (defwindow cal
                       :monitor ${wayland.eww-monitor}
            	   :geometry (geometry :x "35px"
            		               :y "5px"
            		               :width "300px"
            		               :height "100px"
            		               :anchor "bottom right")
            	   :stacking "fg"
            	   :exclusive "false"
            	   :focusable "false"
            	   (cal-widget))

            (defwindow tz
                       :monitor ${wayland.eww-monitor}
            	   :geometry (geometry :x "35px"
            		               :y "5px"
            		               :width "250px"
            		               :height "70px"
            		               :anchor "bottom right")
            	   :stacking "fg"
            	   :exclusive "false"
            	   :focusable "false"
            	   (timezone-widget))

            (defwindow cpu
                       :monitor ${wayland.eww-monitor}
            	   :geometry (geometry :x "350px"
            		               :y "10px"
            		               :width "600px"
            		               :height "450px"
            		               :anchor "bottom left")
            	   :stacking "fg"
            	   :exclusive "false"
            	   :focusable "false"
            	   (cpu-widget))

            (defwindow  events
                 :monitor ${wayland.eww-monitor}
      	         :geometry (geometry :x "700px"
      		                           :y "5px"
      		                           :width "400px"
      		                           :height "100px"
      		                           :anchor "bottom left")
      	         :stacking "fg"
      	         :exclusive "false"
      	         :focusable "false"
      	         (events-widget))

            (defwindow statusbar
               :monitor ${wayland.eww-monitor}
               :geometry (geometry :x "0%"
            	               :y "5px"
            	               :width "98%"
            	               :height "40px"
            	               :anchor "bottom center")
               :stacking "fg"
               :exclusive "true"
               :focusable "false"
               (statusbar-widget
                 :showBattery ${if wayland.showBattery then "true" else "false"}
                 :showTV ${if wayland.showTV then "true" else "false"}
                 :showMail ${if wayland.showMail then "true" else "false"}
                 :showCalendar ${if wayland.showCalendar then "true" else "false"}
               ))
    '';

    programs.eww = {
      enable = true;
      package = pkgs.eww;
      configDir = ./../../../config/eww;
    };

    systemd.user.services.eww = {
      Unit = {
        Description = "ElKowar's Wacky Widgets";
        After = [ "sops-nix.service" "eww.service" ];
      };

      Install = { WantedBy = [ "hyprland-session.target" ]; };

      Service = {
        ExecStart = "${pkgs.eww}/bin/eww daemon --no-daemonize";
        ExecStartPost = "${pkgs.eww}/bin/eww open statusbar";

        Restart = "on-failure";
      };
    };
  };
}
