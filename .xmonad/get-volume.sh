#!/bin/bash

vol=$(amixer -D pulse sget Master | awk -F'[]%[]' '/%/ {if ($5 == "off") { print "Muted" } else { print $2 "%" }}' | head -n 1)

echo $vol

exit 0
