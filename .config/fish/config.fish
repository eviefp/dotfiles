# Path to Oh My Fish install.
set -q XDG_DATA_HOME
  and set -gx OMF_PATH "$XDG_DATA_HOME/omf"
  or set -gx OMF_PATH "$HOME/.local/share/omf"

# Load Oh My Fish configuration.
source $OMF_PATH/init.fish
function clip
    scrot -s ~/screenshot.png
    xclip -selection clipboard -t image/png ~/screenshot.png
    rm ~/screenshot.png
end

function re
    emacs25 --insecure . > /dev/null ^ /dev/null &
end

function fixUI
    xrandr --output HDMI-0 --scale 1.5x1.5
    feh --bg-scale /usr/share/backgrounds/haskell.jpg
end

set -x PATH $HOME/bin $HOME/bin/purescript $HOME/.local/bin $HOME/.local/bin/FlameGraph $HOME/.cargo/bin $PATH $HOME/hems/bin
set -x GEM_HOME $HOME/gems
