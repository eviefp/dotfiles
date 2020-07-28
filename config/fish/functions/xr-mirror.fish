function xr-mirror -d "Mirror" --argument-names 'mode'
    if count $argv > /dev/null
        switch $mode
        case 'off'
            xrandr --output HDMI1 --off
            xrandr --output eDP1 --mode 2560x1440
        case '*'
        xrandr --output eDP1 --mode 1920x1080 --output HDMI1 --mode 1920x1080 --same-as eDP1
        end
    else
        xrandr --output eDP1 --mode 1920x1080 --output HDMI1 --mode 1920x1080 --same-as eDP1
    end
    fixUI
end

