unclutter &
xbanish &
numlockx &

THE_WM="$HOME/.xmonad/xmonad-$(uname -m)-$(uname | tr '[:upper:]' '[:lower:]')"
#THE_WM="$HOME/.cabal/bin/xmonad"

# configuire x environment
xrdb ~/.Xresources
xmodmap ~/.Xmodmap
xset r rate 220 60

# allow local networked connections to X server
xhost +local:

# update dbus x display thingy
dbus-update-activation-environment DISPLAY XAUTHORITY

exec "$THE_WM"
