#!/bin/bash

vol=$(amixer -D pulse sget Capture | awk -F'[]%[]' '/%/ {if ($5 == "off") { print "Muted" } else { print "On" }}' | head -n 1)

echo $vol

exit 0
