function ec -d "emacsclient" --argument-names 'file'
  emacsclient -c -d ":0" $mode
end