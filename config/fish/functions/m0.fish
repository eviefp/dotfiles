function m0 -d "main monitor setup"
    xrandr --output HDMI-0 --off
    xrandr --output DP-0 --mode 1920x1080 --rate 239.76 --left-of DP-2
    xrandr --output DP-2 --mode 1920x1080 --rate 239.76 --primary
    fixUI
end
