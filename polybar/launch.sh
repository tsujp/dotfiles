#!/usr/bin/env bash

# terminate already running bar instances
# killall -q polybar

# wait until the processes have been shut down
# while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

declare -A __arrangement=(["DisplayPort-1"]="mm-top")

for monitor in "${!__arrangement[@]}"; do
  while IFS=',' read -ra barlist; do
    for bar in "${barlist[@]}"; do
      monitor="$monitor" polybar --reload "$bar" &
    done
  done <<< "${__arrangement[$monitor]}"
done
