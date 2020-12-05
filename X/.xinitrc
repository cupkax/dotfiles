#! /bin/sh
## .XResources
userresources=$HOME/.Xresources
sysresources=/etc/X11/xinit/.Xresources

# merge in defaults and keymaps
if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

## Launch dbus 
if [ -d /etc/X11/xinit/xinitrc.d ] ; then

    for f in /etc/X11/xinit/xinitrc.d/?* ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

#undervolt
undervolt --core -120 --cache -120

# Lower the brightness to a minimum
xbacklight -set 30

# Disable mouse acceleration
xset m 0 0

# Load XSettings
xsettingsd &


## DE / WM 
# i3
#exec i3

# GNOME
export XDG_SESSION_TYPE=x11
export GDK_BACKEND=x11
exec gnome-session #--session=awesome
