#!/bin/sh
xinput --set-prop 'SynPS/2 Synaptics TouchPad' 'Trackpad Sensitivity' 0.4  &
xsetwacom set "Wacom Pen and multitouch sensor Pen stylus" Button 2 3 &
xsetwacom set "Wacom Pen and multitouch sensor Pen stylus" TabletPCButton off
