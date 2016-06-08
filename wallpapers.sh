#!/bin/bash

str=$(cat /home/ben/.wallpaper/toggle)
pic="/home/ben/.wallpaper/*$str.jpg"

feh --randomize --bg-scale $pic
