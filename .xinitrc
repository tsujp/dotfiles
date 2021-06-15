numlockx &

THE_WM="/home/tsujp/.xmonad/xmonad-$(uname -m)-$(uname | tr '[:upper:]' '[:lower:]')"

exec "$THE_WM" & PID="$!"
sleep 0.05

if [ $(pidof "$THE_WM") ]; then
  xrdb ~/.Xresources
  xmodmap ~/.Xmodmap
  xset r rate 200 50
fi

wait "$PID"
