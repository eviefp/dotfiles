function clip -d "Mark screenshot region and copy to clipboard"
  scrot -s ~/.screenshot.png
  xclip -selection clipboard -t image/png ~/.screenshot.png
  rm ~/.screenshot.png
end

