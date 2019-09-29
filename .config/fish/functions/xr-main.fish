function xr-main -d "Main screen xrandr" --argument-names 'mode'
    if count $argv > /dev/null
        switch $mode
        case 'off'
            xrandr --output eDP1 --off
        case '*'
	    xrandr --output eDP1 --mode 2560x1440 --right-of DP2
        end
    else
        xrandr --output eDP1 --mode 2560x1440 --right-of DP2
    end
    fixUI
end

