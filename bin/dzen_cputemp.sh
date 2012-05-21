#!/bin/sh
#
# A script that prints CPU temperature for all cores into dzen
#echo -e "$TEMP1°C $TEMP2°C $TEMP3°C $TEMP4°C"


FN='-xos4-terminus-*-*-*-*-12-*-*-*-*-*-*-*'
BG='#333333'
FG='white'
W=640
H=22
X=2560
Y=0

#CAPTION="^fg(violet)^i(/home/lasseb/.share/icons/dzen/cpu.xbm)^fg(white)"
INTERVAL=5
CORE0_TEMP=/sys/class/hwmon/hwmon1/device/temp1_input

while true; do
    CORE0="`cat $CORE0_TEMP`"
    TEMP1="`expr $CORE0 / 1000`"
    echo -e "$TEMP1"
    sleep $INTERVAL;
done #| dzen2 -ta c -tw $W -h $H -x $X -y $Y -fg $FG -bg $BG -fn $FN
