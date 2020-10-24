function xr-home -d "Home xrandr setup" --argument-names 'mode'
    if count $argv > /dev/null
        switch $mode
        case 'off'
            xrandr --output DP-2 --off
        case '*'
	    xrandr --output DP-2 --mode 3840x2160 --primary --left-of eDP-1
        end
    else
        xrandr --output DP-2 --mode 3840x2160 --primary --left-of eDP-1
    end
    fixUI
end
