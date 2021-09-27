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

# if helpers exist, load them
[ -e "$XDG_CONFIG_HOME/shell-profiles/_helpers.sh" ] && {
  source "$XDG_CONFIG_HOME/shell-profiles/_helpers.sh"
}

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

# save vanilla path to prevent endless appends when reloading the shell config
[ -z "$_FIRST_PATH" ] && export _FIRST_PATH="$PATH"
[ "$_FIRST_PATH" != "$PATH" ] && export PATH="$_FIRST_PATH"

# set the path
[ -d "$HOME"/.local/bin ] && PATH="$PATH:$HOME"/.local/bin

export PATH="\
$HOME/bin:\
$HOME/bin/$(hostname):\
$HOME/bin/$(hostname)/$(whoami):\
$HOME/bin/$(hostname)/$(whoami)/$(uname -m):\
$PATH\
"

# if on a 'host' we only ever configure with wayland set the appropriate
# environment variables
_wayland_enabled ()
{
  set -f
  local wayland_hosts='river:hikari:gnome'
  IFS=':'
  set -- $wayland_hosts
  set +f

  for wh in "$@"; do
    if [ "$XDG_SESSION_DESKTOP" = "$wh" ]; then
      puts_step "In XDG desktop '%s', setting Wayland vars." "$wh"

      # wayland only terminal
      export TERM='foot'

      # enable wayland preference / disable xwayland
      export MOZ_ENABLE_WAYLAND=1
      export CLUTTER_BACKEND='wayland'
      export QT_QPA_PLATFORM='wayland-egl'
      export ECORE_EVAS_ENGINE='wayland-egl'
      export ELM_DISPLAY='wl'
      export ELM_ENGINE='wayland_egl'
      export SDL_VIDEODRIVER='wayland'
      export _JAVA_AWT_WM_NONREPARENTING=1
      export NO_AT_BRIDGE=1
    fi
  done
}

_wayland_enabled
