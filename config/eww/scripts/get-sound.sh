#!/usr/bin/env bash

status=`amixer get Master | tail -n1 | grep -wo 'on'`

if [[ "$status" == "on" ]]; then
  volume=`amixer get Master | tail -n1 | awk -F ' ' '{print $5}' | tr -d '[]%'`
else
  volume="0"
fi

if [[ "$volume" -gt "0" ]]; then
    icon="./assets/volume.png"
else
    icon="./assets/mute.png"
fi

echo "{\"volume\": \"$volume\", \"icon\": \"$icon\"}"
