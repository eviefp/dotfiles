function rt -d "Reset touchpad, useful after sleep"
  sudo modprobe -r psmouse
  sudo modprobe psmouse
end
