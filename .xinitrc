#!/bin/bash

# .xinitrc

# Load Resources

xrdb -merge ~/.Xdefaults &

# Set up an icon tray

trayer --edge top --alpha 1 --align right --SetDockType true --SetPartialStrut true --expand true --width 4 --height 9 --widthtype percent --transparent true --tint 0x000000 &

# Set background and cursor

xsetroot -cursor_name left_ptr &
xset m 0 0 &
setxkbmap no &
xmodmap ~/.Xmodmap &

#find $HOME/Pictures/WideWallpaper -type f -name '*.jpg' -o -name '*.png' | shuf -n 1 | xargs feh --bg-scale &
nitrogen --restore &
# Launch apps
# xscreensaver &
ossxmix -b &
thunar --daemon &
deluge &
pulseaudio -D &
urxvtd -q -f -o &
pidgin &
numlockx &
fans 40 &
xmonad
