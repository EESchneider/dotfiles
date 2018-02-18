#!/bin/sh

sh ~/.xstartup
polybar -r cherry &
unclutter &
setxkbmap -option caps:none
xset -r 66 # disable CapsLock repeat
xmodmap "$HOME/.Xmodmap"
exec i3
