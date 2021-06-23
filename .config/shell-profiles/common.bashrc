#!/usr/bin/env bash

# load "account" bashrc, if it exists
[ -e "$HOME/.config/shell-profiles/$(hostname)-$(whoami).bashrc" ] && source "$HOME/.config/shell-profiles/$(hostname)-$(whoami).bashrc"
