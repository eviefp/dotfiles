function tv -d "Home xrandr setup" --argument-names 'mode'
    if count $argv > /dev/null
        switch $mode
        case 'off'
            xrandr --output HDMI-0 --off
        case '*'
            xrandr --output HDMI-0 --mode 1920x1080 --right-of DP-2
        end
    else
        xrandr --output HDMI-0 --mode 1920x1080 --right-of DP-2
    end
    fixUI
end
