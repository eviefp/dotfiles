#!/usr/bin/env bash

num_monitors=`hyprctl monitors -j | jq length`

if [ "$num_monitors" -lt "4" ]; then
  hyprctl keyword monitor "HDMI-A-1,1920x1080@60,0x395,1"
else
  hyprctl keyword monitor "HDMI-A-1,disabled"
fi
