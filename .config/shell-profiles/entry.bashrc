#!/usr/bin/env bash

# Entry `bashrc`. Will load base and the matching host-user combination
#   `bashrc` if it exists.

# Base.
. "${HOME}/.config/shell-profiles/base.bashrc"

# host-user combination `bashrc` if it exists.
[ -f "${HOME}/.config/shell-profiles/${HOSTNAME}-${USER}.bashrc" ] && {
  . "${HOME}/.config/shell-profiles/${HOSTNAME}-${USER}.bashrc"
}

# TODO: Remove this and put it elsewhere.
# pnpm
# export PNPM_HOME="/Users/tsujp/.local/share/pnpm"
# export PATH="$PNPM_HOME:$PATH"
# pnpm end

# TODO: Nargo please respect XDG.
export NARGO_HOME="/Users/tsujp/.nargo"

export PATH="$PATH:$NARGO_HOME/bin"
# TODO: Put this elsewhere.
export PATH="/Users/tsujp/.local/cache/.bun/bin:$PATH"
