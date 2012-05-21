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
CORE0_TEMP=/sys/class/hwmon/hwmon1/device/temp2_input
CORE1_TEMP=/sys/class/hwmon/hwmon1/device/temp3_input
CORE2_TEMP=/sys/class/hwmon/hwmon1/device/temp4_input
CORE3_TEMP=/sys/class/hwmon/hwmon1/device/temp5_input

while true; do
    CORE0="`cat $CORE0_TEMP`"
    TEMP1="`expr $CORE0 / 1000`"
    CORE1="`cat $CORE1_TEMP`"
    TEMP2="`expr $CORE1 / 1000`"
    CORE2="`cat $CORE2_TEMP`"
    TEMP3="`expr $CORE2 / 1000`"
    CORE3="`cat $CORE3_TEMP`"
    TEMP4="`expr $CORE3 / 1000`"
    ALL="`expr $TEMP1 + $TEMP2 + $TEMP3 + $TEMP4`"
    AVG="`expr $ALL / 4`"
#    echo -n "$CAPTION "
    echo -e "$AVG"
    sleep $INTERVAL;
done #| dzen2 -ta c -tw $W -h $H -x $X -y $Y -fg $FG -bg $BG -fn $FN
