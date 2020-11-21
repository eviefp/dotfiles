function m0 -d "main monitor setup"
    xrandr --output DP2 --off
    xrandr --output HDMI1 --off
    xrandr --output eDP1 --mode 2560x1440
    fixUI
end
