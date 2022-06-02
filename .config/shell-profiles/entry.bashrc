#!/usr/bin/env bash

# Entry `bashrc`. Will load base and the matching host-user combination
#   `bashrc` if it exists.

# Base.
. "$HOME/.config/shell-profiles/base.bashrc"

# host-user combination `bashrc` if it exists.
[ -f "$HOME/.config/shell-profiles/$(hostname)-$(whoami).bashrc" ] && {
  . "$HOME/.config/shell-profiles/$(hostname)-$(whoami).bashrc"
}
