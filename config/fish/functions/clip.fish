function clip -d "Mark screenshot region and copy to clipboard"
  rm ~/.screenshot.png
  scrot -s ~/.screenshot.png
  xclip -selection clipboard -t image/png ~/.screenshot.png
end

