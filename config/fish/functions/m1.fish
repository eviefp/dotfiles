function m1 -d "1 monitor setup"
    xrandr --output DP2 --mode 1920x1080 --primary --left-of eDP1
    xrandr --output HDMI1 --off
    xrandr --output eDP1 --off
    fixUI
end
