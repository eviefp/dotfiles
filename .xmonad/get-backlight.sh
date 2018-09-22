#!/bin/bash

backlight=$(xbacklight | awk '{printf "%d", $1}')

echo $backlight

exit 0
