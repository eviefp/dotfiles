#!/usr/bin/env bash

apikey=`cat /var/run/secrets/weatherKey`

weather=`http 'http://api.weatherapi.com/v1/current.json?key='$apikey'&q=Bucharest&aqi=yes'`

icon=`echo $weather | jq -r .current.condition.icon`

http http:$icon -o /home/evie/.config/eww-extras/weather_tmp.png

fileType=`file -i /home/evie/.config/eww-extras/weather_tmp.png`
echo $fileType
if [[ "$fileType" == "/home/evie/.config/eww-extras/weather_tmp.png: image/png; charset=binary" ]]; then
    mv /home/evie/.config/eww-extras/weather_tmp.png /home/evie/.config/eww-extras/weather.png
    echo "success" >> /home/evie/.config/eww-extras/last-run
else
  rm /home/evie/.config/eww-extras/weather_tmp.png
fi


date >> /home/evie/.config/eww-extras/last-run


echo $weather
