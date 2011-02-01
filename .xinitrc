#!/bin/bash

# .xinitrc

# Load Resources

xrdb -merge ~/.Xdefaults &
setxkbmap no &

# Set up an icon tray
trayer --edge top --alpha 1 --align right --SetDockType true --SetPartialStrut true --expand true --width 4 --height 17 --widthtype percent --transparent true --tint 0x303030 &

# Start urxvt daemon
urxvtd -q -f -o &

# Set background and cursor
xsetroot -cursor_name left_ptr &

# Set mouse acceleration and threshold (m acc thr) and screensaver (s on/off) and dpms off
xset m 0 0 s off -dpms &

# Set the interval of inactivity, after which the screen will be automatically blanked
setterm -blank 0 &

#find $HOME/Pictures/WideWallpaper -type f -name '*.jpg' -o -name '*.png' | shuf -n 1 | xargs feh --bg-scale &
nitrogen --restore &

# Launch apps
# xscreensaver &
#thunar --daemon &
idesk &
udiskie &
ossxmix -b &
wicd-client &
numlockx &
(sleep 3 && xmodmap -e "clear Lock") &
(sleep 3 && xmodmap /home/lasseb/.Xmodmap) &
(sleep 3 && urxvt -name IRC) &
#pidgin &
empathy &
#xcompmgr -sfF &
#xcompmgr -c -t-5 -l-5 -r4.2 -o.55 &
cairo-compmgr &
exec dbus-launch ck-launch-session xmonad
