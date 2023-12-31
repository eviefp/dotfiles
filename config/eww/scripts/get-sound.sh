#!/usr/bin/env bash

sound=`amixer get Master | tail -n1 | grep -wo 'on'`

if [[ "$sound" == "on" ]]; then
  svolume=`amixer get Master | tail -n1 | awk -F ' ' '{print $5}' | tr -d '[]%'`
else
  svolume="0"
fi

if [[ "$svolume" -gt "0" ]]; then
    sicon="./assets/volume.png"
else
    sicon="./assets/mute.png"
fi

microphone=`amixer get Capture | tail -n1 | grep -wo 'on'`

if [[ "$microphone" == "on" ]]; then
  mvolume=`amixer get Capture | tail -n1 | awk -F ' ' '{print $4}' | tr -d '[]%'`
else
  mvolume="0"
fi

if [[ "$mvolume" -gt "0" ]]; then
    micon="./assets/mic-volume.png"
else
    micon="./assets/mic-mute.png"
fi

echo \
    "{\"svolume\": \"$svolume\",\
      \"sicon\": \"$sicon\",\
      \"mvolume\": \"$mvolume\",\
      \"micon\": \"$micon\"}"
