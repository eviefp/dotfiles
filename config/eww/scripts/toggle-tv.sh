#!/usr/bin/env bash

num_monitors=`hyprctl monitors -j | jq length`

if [ "$num_monitors" -lt "4" ]; then
  wlr-randr --output HDMI-A-2 --mode 1920x1080@60 --pos 5760,0 --on
else
  wlr-randr --output HDMI-A-2 --off
fi
