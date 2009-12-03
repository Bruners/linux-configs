#!/bin/bash
# .xinitrc

# Load Resources

xrdb -merge ~/.Xdefaults &

# Set up an icon tray

trayer --edge top --alpha 1 --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --height 9 --widthtype percent --transparent true --tint 0x000000 &

# Set background and cursor

xsetroot -cursor_name left_ptr &
xset m 0 0 &
setxkbmap no &
xmodmap ~/.Xmodmap &

find $HOME/Pictures/WideWallpaper -type f -name '*.jpg' -o -name '*.png' | shuf -n 1 | xargs feh --bg-scale &

# Launch apps
# xscreensaver &
ossxmix -b &
gigolo &
deluge &
pulseaudio -D &
urxvtd -q -f -o &
pidgin &
xmonad
