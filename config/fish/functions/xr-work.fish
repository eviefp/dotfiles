function xr-work -d "Work xrandr setup" --argument-names 'mode'
    if count $argv > /dev/null
        switch $mode
        case 'off'
            xrandr --output DP2 --off
        case '*'
	    xrandr --output DP2 --mode 2560x1440 --right-of eDP1 
        end
    else
        xrandr --output DP2 --mode 2560x1440 --right-of eDP1
    end
    fixUI
end

