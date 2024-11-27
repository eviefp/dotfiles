#!/usr/bin/env bash

apikey=`cat /var/run/secrets/weatherKey`

weather=`http 'http://api.weatherapi.com/v1/current.json?key='$apikey'&q=Bucharest&aqi=yes'`

icon=`echo $weather | jq -r .current.condition.icon`

http http:$icon -o /home/evie/.config/eww-extras/weather.png

date > /home/evie/.config/eww-extras/last-run

echo $weather
