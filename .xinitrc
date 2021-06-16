numlockx &

THE_WM="/home/tsujp/.xmonad/xmonad-$(uname -m)-$(uname | tr '[:upper:]' '[:lower:]')"

# configuire x environment
xrdb ~/.Xresources
xmodmap ~/.Xmodmap
xset r rate 200 50

# allow local networked connections to X server
xhost +local:

# update dbus x display thingy
dbus-update-activation-environment DISPLAY XAUTHORITY

exec "$THE_WM"
