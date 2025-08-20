#!/usr/bin/env nu

# { text, tooltip }

hyprctl monitors -j
  | jq '.[] | {id, name, workspace:.activeWorkspace.id}'
  | from json
  | reduce --fold {} {|
