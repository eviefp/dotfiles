#!/usr/bin/env bash

berlin=`TZ=Europe/Berlin date +%H:%M:%S`
ny=`TZ=America/New_York date +%H:%M:%S`
la=`TZ=America/Los_Angeles date +%H:%M:%S`

echo '[ {"name": "Los Angeles", "time": "'$la'"}, {"name": "New York", "time": "'$ny'"}, {"name": "Berlin", "time": "'$berlin'"} ]'
