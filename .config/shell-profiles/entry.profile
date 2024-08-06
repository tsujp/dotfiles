#!/bin/sh

# Entry `profile`. Will load base and the matching host-user combination
#   `profile` if it exists.

# macOS annoyingly always launches a login shell. We could change this but
#   I feel like doing this in POSIX Shell instead so I know I don't have to
#   second guess myself down the track around "is it reading the config
#   changes I made to do a non-login shell?" etc. If we always further
#   load the appropriate `bashrc` __here__ I don't have to worry.

# Base.
. "$HOME/.config/shell-profiles/base.profile"

# host-user combination `profile` if it exists.
[ -f "$HOME/.config/shell-profiles/$(uname -n)-$(id -un).profile" ] && {
  . "$HOME/.config/shell-profiles/$(uname -n)-$(id -un).profile"
}

# If macOS load `entry.bashrc`, see top-of-file note on why.
[ "$(uname -s)" = "Darwin" ] && {
  . "$HOME/.config/shell-profiles/entry.bashrc"
}
# . "$HOME/.cargo/env"
