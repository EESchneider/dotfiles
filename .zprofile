export XDG_CONFIG_HOME="$HOME/.config"
if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -le 3 ]; then
   exec startx
fi
