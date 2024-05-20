#!/bin/sh

# Common base for all systems I use. Anything specific to a particular
#   user and host combination would go in it's own profile. The sole
#   exception is the general case for macOS (e.g. adding macports to
#   the path here) since it's _so_ common.

# Base.
export XDG_CONFIG_HOME="$HOME/.config";
export XDG_DATA_HOME="$HOME/.local/share";
export XDG_CACHE_HOME="$HOME/.local/cache";
export XDG_DESKTOP_DIR="$HOME/desktop";
export XDG_DOCUMENTS_DIR="$HOME/documents";
export XDG_DOWNLOAD_DIR="$HOME/downloads";
export XDG_MUSIC_DIR="$HOME/music";
export XDG_PICTURES_DIR="$HOME/pictures";
export XDG_VIDEOS_DIR="$HOME/videos";

# TODO: Find out which one is used for keyboard layout and set others
#       to sane English i.e. en_GB. American spelling is S T U P I D.
# Locale.
: ${LANG:="en_US.UTF-8"};     export LANG
: ${LANGUAGE:="en"};          export LANGUAGE
: ${LC_CTYPE:="en_US.UTF-8"}; export LC_CTYPE
: ${LC_ALL:="en_US.UTF-8"};   export LC_ALL

# Editors.
export EDITOR='hx'
export VISUAL='hx'
export ALTERNATE_EDITOR='vim'

# Paging.
export PAGER='less -SFw'
export SYSTEMD_PAGER='less -SFw'
export MANPAGER="$PAGER"

# GnuPG.
export GNUPGHOME="$XDG_CONFIG_HOME/gnupg"
export PINEENTRY_USER_DATA='USE_CURSES=1'
export GPG_TTY="$(tty)"

# WGet.
export WGETRC="$XDG_CONFIG_HOME/wgetrc"

# History.
export HISTFILE="$XDG_DATA_HOME/history"
export LESSHISTFILE='-'
export HISTCONTROL=ignoredups:erasedups
export HISTSIZE=65536

# Save vanilla path to prevent endless appends when reloading the shell config.
[ -z "$__tsujp_first_path" ] && export __tsujp_first_path="$PATH"
[ "$__tsujp_first_path" != "$PATH" ] && export PATH="$__tsujp_first_path"

# Set PATH.

# Two special cases, (1) for local precedence, (2) for macOS.
[ -d "$HOME"/.local/bin ] && PATH="$HOME/.local/bin:$PATH"
[ -d /opt/local/bin ] && PATH="/opt/local/bin:$PATH"

# Regular pattern.
export PATH="\
$HOME/bin:\
$HOME/bin/$(hostname):\
$HOME/bin/$(hostname)/$(whoami):\
$HOME/bin/$(hostname)/$(whoami)/$(uname -m):\
$PATH\
"

# TODO: Put this elsewhere.
# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix
