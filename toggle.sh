#!/bin/bash

str=$(cat /home/ben/.wallpaper/toggle)

if [ "$str" == "BG" ]
then
  echo "PIC" > /home/ben/.wallpaper/toggle
  echo "Toggled to pics!"
  /home/ben/.wallpaper/wallpapers.sh
else
  echo "BG" > /home/ben/.wallpaper/toggle
  echo "Toggled to backgrounds!"
  /home/ben/.wallpaper/wallpapers.sh
fi
