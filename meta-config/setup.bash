#!/bin/bash
dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
source "$dir"/sontek/sontek.bash

select_options "$dir/defs"
# choice=$?
# echo "Choosen index = $choice"
