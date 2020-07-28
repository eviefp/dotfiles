#!/bin/bash

path=/sys/class/power_supply/BAT0

status=$(cat $path/status)
level=$(cat $path/capacity)

echo $status "($level%)"
