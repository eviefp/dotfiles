#!/usr/bin/env bash

num_monitors=`hyprctl monitors -j | jq length`

if [ "$num_monitors" -lt "4" ]; then
  wlr-randr --output HDMI-A-2 --on
else
  wlr-randr --output HDMI-A-2 --off
fi
