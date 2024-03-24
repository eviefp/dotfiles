#!/usr/bin/env bash

apikey=`cat ~/.secrets/weather.key`

weather=`http 'http://api.weatherapi.com/v1/current.json?key='$apikey'&q=Bucharest&aqi=yes'`

icon=`echo $weather | jq -r .current.condition.icon`

http http:$icon -o /home/evie/.config/eww-extras/weather.png

echo $weather
