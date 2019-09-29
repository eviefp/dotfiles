function xr-home -d "Home xrandr setup" --argument-names 'mode'
    if count $argv > /dev/null
        switch $mode
        case 'off'
            xrandr --output DP2 --off
        case '*'
	    xrandr --output DP2 --mode 2560x1440 --left-of eDP1
        end
    else
        xrandr --output DP2 --mode 2560x1440 --left-of eDP1
    end
    fixUI
end

