#!/bin/sh

# base
export XDG_CONFIG_HOME="$HOME/.config";
export XDG_DATA_HOME="$HOME/.local/share";
export XDG_CACHE_HOME="$HOME/.local/cache";
export XDG_DESKTOP_DIR="$HOME/desktop";
export XDG_DOCUMENTS_DIR="$HOME/documents";
export XDG_DOWNLOAD_DIR="$HOME/downloads";
export XDG_MUSIC_DIR="$HOME/music";
export XDG_PICTURES_DIR="$HOME/pictures";
export XDG_VIDEOS_DIR="$HOME/videos";

# locale
: ${LANG:="en_US.UTF-8"};     export LANG
: ${LANGUAGE:="en"};          export LANGUAGE
: ${LC_CTYPE:="en_US.UTF-8"}; export LC_CTYPE
: ${LC_ALL:="en_US.UTF-8"};   export LC_ALL

# editors
export ALTERNATE_EDITOR='vim'
export EDITOR='vim'
export VISUAL='vim'

# paging
export PAGER='less -SFw'
export SYSTEMD_PAGER='less -SFw'
export MANPAGER="$PAGER"

# gpg / gnupg
export GNUPGHOME="$XDG_CONFIG_HOME/gnupg"
export GPG_TTY="$(tty)"
export PINEENTRY_USER_DATA='USE_CURSES=1'

# wget
export WGETRC="$XDG_CONFIG_HOME/wgetrc"

# history
export HISTFILE="$XDG_DATA_HOME/history"
export LESSHISTFILE='-'
export HISTCONTROL=ignoredups:erasedups
export HISTSIZE=65536

# path
export PATH="$PATH:\
$HOME/bin/$(hostname):\
$HOME/bin/$(hostname)/$(whoami):\
$HOME/bin/$(hostname)/$(whoami)/$(uname -m):\
$HOME/bin:\
"

