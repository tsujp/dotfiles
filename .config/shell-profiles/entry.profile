#!/bin/sh

# Entry `profile`. Will load base and the matching host-user combination
#   `profile` if it exists.

# macOS annoyingly always launches a login shell. We could change this but
#   I feel like doing this in POSIX Shell instead so I know I don't have to
#   second guess myself down the track around "is it reading the config
#   changes I made to do a non-login shell?" etc. If we always further
#   load the appropriate `bashrc` __here__ I don't have to worry.

# TODO: Will these actually carry over if something is setting them to empty?
if [ ! "$HOSTNAME" ]; then
    export HOSTNAME="$(uname -n)"
fi

if [ ! "$USER" ]; then
    export USER="$(id -un)"
fi

# Base.
. "${HOME}/.config/shell-profiles/base.profile"

# host-user combination `profile` if it exists.
[ -f "${HOME}/.config/shell-profiles/${HOSTNAME}-${USER}.profile" ] && {
  . "${HOME}/.config/shell-profiles/${HOSTNAME}-${USER}.profile"
}

# If macOS load `entry.bashrc`, see top-of-file note on why.
[ "$(uname -s)" = 'Darwin' ] && {
  . "$HOME/.config/shell-profiles/entry.bashrc"
}
