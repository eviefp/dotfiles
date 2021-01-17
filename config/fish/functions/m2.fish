function m2 -d "2 monitor setup"
    xrandr --output HDMI-0 --mode 1920x1080 --primary
    xrandr --output DP-2 --mode 1920x1080 --right-of HDMI-0
    fixUI
end
