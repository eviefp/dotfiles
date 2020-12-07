function m2 -d "2 monitor setup"
    xrandr --output DP2 --mode 1920x1080 --primary --left-of eDP1
    xrandr --output HDMI1 --mode 1920x1080 --left-of DP2
    xrandr --output eDP1 --off
    fixUI
end
