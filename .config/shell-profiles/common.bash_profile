#!/usr/bin/env bash

# load home .profile
[ -e "$HOME/.profile" ] && source "$HOME/.profile"

# load common bashrc for "account" profiles
[ -e "$HOME/.config/shell-profiles/common.bashrc" ] && source "$HOME/.config/shell-profiles/common.bashrc"

