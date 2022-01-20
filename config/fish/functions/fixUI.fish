function fixUI -d "Fix the background image"
  killall -9 feh
  feh --bg-scale ~/desktop.jpg
end
