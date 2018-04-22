#!/bin/sh
for monitor in $(bspc query -M --names); do
	monitors+=("$monitor")
done
#echo ${#monitors[@]} # Number of coonnected Monitors

if [ ${#monitors[@]} == 1 ]; then
	# Single Monitor Setup
#	xrandr --output ${monitors[0]} --primary --mode 1920x1080 --pos 0x0
	bspc monitor ${monitors[0]} -d 1 2 3 4 5 6 7 8 9 10
elif xrandr | grep ' connected' | grep -q "DP2-2" &&  xrandr | grep ' connected' | grep -q "eDP1"; then
	# Home Docking Setup
	xrandr --output DP2-2 --primary --mode 2560x1080 --pos 0x0
	xrandr --output eDP1 --mode 1920x1080 --pos 2560x0
	bspc monitor DP2-2 -d 1 2 3 4 5 6 7 8
	bspc monitor eDP1 -d 9 10
else
	for monitor in $(bspc query -M); do
		bspc monitor $monitor -d 1 2 3 4 5
	done
fi

