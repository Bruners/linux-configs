#!/bin/sh
#
# A script that prints CPU temperature for all cores into dzen
#
 
FN='-xos4-terminus-*-*-*-*-12-*-*-*-*-*-*-*'
BG='#333333'
FG='#BBBBBB'
W=640
H=22
X=2560
Y=0

CAPTION="^fg(#eeffcc)^i(/home/lasseb/.share/icons/dzen/cpu.xbm)^fg()"
INTERVAL=4
CORE0_TEMP=/sys/devices/platform/coretemp.0/temp1_input
CORE1_TEMP=/sys/devices/platform/coretemp.1/temp1_input
CORE2_TEMP=/sys/devices/platform/coretemp.2/temp1_input
CORE3_TEMP=/sys/devices/platform/coretemp.3/temp1_input

while true; do
	CORE0="`cat $CORE0_TEMP`"
	TEMP1="`expr $CORE0 / 1000`"
	CORE1="`cat $CORE1_TEMP`"
	TEMP2="`expr $CORE1 / 1000`"
	CORE2="`cat $CORE2_TEMP`"
	TEMP3="`expr $CORE2 / 1000`"
	CORE3="`cat $CORE3_TEMP`"
	TEMP4="`expr $CORE3 / 1000`"
	echo -n "$CAPTION "
	echo -e "$TEMP1°C $TEMP2°C $TEMP3°C $TEMP4°C"
	sleep $INTERVAL;
done #| dzen2 -ta c -tw $W -h $H -x $X -y $Y -fg $FG -bg $BG -fn $FN
