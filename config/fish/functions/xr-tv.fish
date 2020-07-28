function xr-tv -d "Home xrandr setup" --argument-names 'mode'
    if count $argv > /dev/null
        switch $mode
        case 'off'
            xrandr --output HDMI1 --off
        case '*'
	    xrandr --output HDMI1 --mode 1920x1080 --left-of eDP1
        end
    else
        xrandr --output HDMI1 --mode 1920x1080 --left-of eDP1
    end
    fixUI
end

