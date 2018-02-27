#!/bin/sh
sleep 3
echo `date` >> /home/user/monitor.log
if xrandr | grep ' connected' | grep -q "DP2-2"; then
	echo "DP2-2 is plugged in" >> /home/user/monitor.log
	xrandr --output DP2-2 --primary --mode 2560x1080 --pos 0x0
	xrandr --output eDP1 --off
	#xrandr --output eDP1 --mode 1920x1080 --pos 2560x0
	bspc monitor -d I II III IV V VI VII VIII IX X
	#bspc monitor eDP1 -d VII VIII IX X
else
	echo "No external monitors are plugged in" >> /home/user/monitor.log
	xrandr --output eDP1 --primary --mode 1920x1080 --pos 0x0
	bspc monitor -d I II III IV V VI VII VIII IX X
fi
xrandr -q | grep " connected" | cut -d ' ' -f1 >> /home/user/monitor.log
