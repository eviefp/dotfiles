#!/usr/bin/env bash

num_monitors=`hyprctl monitors -j | jq length`

if [ "$num_monitors" -lt "4" ]; then
  hyprctl keyword monitor "HDMI-A-2,1920x1080@60,5760,1"
else
  hyprctl keyword monitor "HDMI-A-2,disabled"
fi
