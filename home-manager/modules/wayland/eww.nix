{ lib, config, pkgs, ... }:
let
  cfg = config.evie.wayland;
in
{
  imports = [ ];

  config = lib.mkIf cfg.enable {

    home.packages = [
      pkgs.socat
      pkgs.wlr-randr
    ];

    home.file.".config/eww-extras/windows.yuck".text = ''
            (defwindow cal
                       :monitor ${cfg.eww-monitor}
            	   :geometry (geometry :x "60px"
            		               :y "10px"
            		               :width "300px"
            		               :height "200px"
            		               :anchor "bottom right")
            	   :stacking "fg"
            	   :exclusive "false"
            	   :focusable "false"
            	   (cal-widget))

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
      	         :geometry (geometry :x "450px"
      		                           :y "10px"
      		                           :width "300px"
      		                           :height "200px"
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
