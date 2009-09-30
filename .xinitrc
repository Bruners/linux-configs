#!/bin/bash
# .xinitrc

# Load resources

xrdb -merge .Xdefaults

# Set up an icon tray

trayer --edge top --alpha 1 --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --height 11 --transparent true --tint 0x000000 &

# Set background

xsetroot -solid black &
xsetroot -cursor_name left_ptr

find $HOME/Pictures/wallpaper -type f -name '*.jpg' -o -name '*.png' | shuf -n 1 | xargs feh --bg-scale

# Launch apps

#xscreensaver &
xcompmgr &
wicd-client &
gnome-phone-manager &
gnome-power-manager &
bluetooth-applet &

exec xmonad
