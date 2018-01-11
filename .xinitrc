#!/bin/sh

sh ~/.xstartup
polybar -r cherry &
unclutter &
xmodmap ~/.xmodmap
sxhkd &
exec bspwm
# exec xmonad
