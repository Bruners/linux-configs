#!/bin/sh

while true;
do
	find $HOME/Pictures/wallpaper -type f -name '*.jpg' -o -name '*.png' | shuf -n 1 | xargs feh --bg-scale
	sleep 15m
done &
