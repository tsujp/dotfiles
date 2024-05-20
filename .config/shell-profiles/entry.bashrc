#!/usr/bin/env bash

# Entry `bashrc`. Will load base and the matching host-user combination
#   `bashrc` if it exists.

# Base.
. "$HOME/.config/shell-profiles/base.bashrc"

# host-user combination `bashrc` if it exists.
[ -f "$HOME/.config/shell-profiles/$(uname -n)-$(id -un).bashrc" ] && {
  . "$HOME/.config/shell-profiles/$(uname -n)-$(id -un).bashrc"
}

# TODO: Remove this and put it elsewhere.
# . "$HOME/.cargo/env"

# TODO: Remove this and put it elsewhere.
# pnpm
export PNPM_HOME="/Users/tsujp/.local/share/pnpm"
export PATH="$PNPM_HOME:$PATH"
# pnpm end

export NARGO_HOME="/Users/tsujp/.nargo"

export PATH="$PATH:$NARGO_HOME/bin"
