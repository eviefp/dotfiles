#!/usr/bin/env bash

apikey=`cat /var/run/secrets/weatherKey`

weather=`http 'http://api.weatherapi.com/v1/current.json?key='$apikey'&q=Bucharest&aqi=yes'`

icon=`echo $weather | jq -r .current.condition.icon`

http get http:$icon -o /home/evie/.config/eww-extras/weather_tmp.png

now=`date`
fileType=`file -i /home/evie/.config/eww-extras/weather_tmp.png`
if [[ "$fileType" == "/home/evie/.config/eww-extras/weather_tmp.png: image/png; charset=binary" ]]; then
    mv /home/evie/.config/eww-extras/weather_tmp.png /home/evie/.config/eww-extras/weather.png
    echo "$now success " >> /home/evie/.config/eww-extras/last-run
else
  echo "$now *** failure" >> /home/evie/.config/eww-extras/last-run
  echo "  -> weather : $weather" >> /home/evie/.config/eww-extras/last-run
  echo "  -> icon    : $icon" >> /home/evie/.config/eww-extras/last-run
  echo "  -> filetype: $filetype" >> /home/evie/.config/eww-extras/last-run
  echo "\n\n" >> /home/evie/.config/eww-extras/last-run
fi

echo $weather
