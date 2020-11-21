function m1 -d "1 monitor setup"
    xrandr --output DP2 --mode 3840x2160 --primary --left-of eDP1
    xrandr --output HDMI1 --off
    xrandr --output eDP1 --off
    fixUI
end
