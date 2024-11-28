#!/usr/bin/env bash

sinkMute=`pactl get-sink-mute @DEFAULT_SINK@ | choose 1`

if [[ "$sinkMute" == "no" ]]; then
  svolume=`pactl get-sink-volume @DEFAULT_SINK@ | choose 4 | tr -d '[]%'`
else
  svolume="0"
fi

if [[ "$svolume" -gt "0" ]]; then
    sicon="./assets/volume.png"
else
    sicon="./assets/mute.png"
fi

sname=`pactl get-default-sink`

sourceMute=`pactl get-source-mute @DEFAULT_SOURCE@ | choose 1`

if [[ "$sourceMute" == "no" ]]; then
  mvolume=`pactl get-source-volume @DEFAULT_SOURCE@ | choose 4 | tr -d '[]%'`
else
  mvolume="0"
fi

if [[ "$mvolume" -gt "0" ]]; then
    micon="./assets/mic-volume.png"
else
    micon="./assets/mic-mute.png"
fi

mname=`pactl get-default-source`

echo \
    "{\"svolume\": \"$svolume\",\
      \"sicon\": \"$sicon\",\
      \"sname\": \"$sname\",\
      \"mvolume\": \"$mvolume\",\
      \"micon\": \"$micon\",\
      \"mname\": \"$mname\"}"
