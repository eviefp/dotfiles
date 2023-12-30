#!/usr/bin/env bash

hyprctl activewindow -j | jq '.class' | awk '{print tolower($0)}'

socat -u UNIX-CONNECT:/tmp/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock - |
  stdbuf -o0 awk -F '>>|,' -e '/^activewindow>>/ {print tolower($2)}'
