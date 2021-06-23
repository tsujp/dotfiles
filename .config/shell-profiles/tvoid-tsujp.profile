#!/bin/sh

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## NON-XDG-ENV - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# dotnet
export DOTNET_CLI_TELEMETRY_OPTOUT=1

# rust
export CARGO_HOME="$XDG_DATA_HOME/cargo"

# editors
export ALTERNATE_EDITOR='vim'
export EDITOR='emacsclient -ca ""'
export VISUAL='emacs client -ca ""'

