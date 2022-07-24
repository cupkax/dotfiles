#! /bin/sh
## xserver settings
# Lower the brightness to a minimum
xbacklight -set 100

# Disable mouse acceleration
xset m 0 0

# Default keymap
setxkbmap gb

# Load XSettings
xsettingsd &

## DE / WM
# i3
#exec i3

# GNOME
export XDG_SESSION_TYPE=x11
export GDK_BACKEND=x11
exec gnome-session
