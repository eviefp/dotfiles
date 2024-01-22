#!/usr/bin/env bash

val=`eww get procEntry`

if [ $val -eq 8 ]; then
    exit;
else
    eww update procEntry=$((val+1))
fi
