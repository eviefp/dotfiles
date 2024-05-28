{ lib, config, pkgs, ... }:
let
  cfg = config.evie.wayland;
  sshThelxinoe = if cfg.useSshMailCalendar then "ssh thelxinoe " else "";
in
{
  imports = [ ];

  config = lib.mkIf cfg.enable {

    home.packages = [
      pkgs.socat
      pkgs.wlr-randr
    ];

    home.file.".config/eww-extras/mail-calendar-vars.yuck".text = ''
      (defpoll mail-important :initial "0" :interval "10s" "${sshThelxinoe}notmuch count tag:important")
      (defpoll mail-unread :initial "0" :interval "10s" "${sshThelxinoe}notmuch count tag:unread")
      (deflisten cal :initial `{ "title": "Loading...", "time": "00:00", "date": "2050-01-01" }` "~/.config/eww/scripts/get-next-calendar-entry.sh")
      (defpoll events :initial "[]" :interval "30s" "ect upcoming --value 10")
    '';

    home.file.".config/eww-extras/windows.yuck".text = ''
            (defwindow cal
                       :monitor ${cfg.eww-monitor}
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
                       :monitor ${cfg.eww-monitor}
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
                       :monitor ${cfg.eww-monitor}
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
                 :monitor ${cfg.eww-monitor}
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
               :monitor ${cfg.eww-monitor}
               :geometry (geometry :x "0%"
            	               :y "5px"
            	               :width "98%"
            	               :height "40px"
            	               :anchor "bottom center")
               :stacking "fg"
               :exclusive "true"
               :focusable "false"
               (statusbar-widget
                 :showBattery ${if cfg.showBattery then "true" else "false"}
                 :showTV ${if cfg.showTV then "true" else "false"}
                 :showMail ${if cfg.showMail then "true" else "false"}
                 :showCalendar ${if cfg.showCalendar then "true" else "false"}
               ))
    '';

    programs.eww = {
      enable = true;
      package = pkgs.eww;
      configDir = ./../../../config/eww;
    };
  };
}
