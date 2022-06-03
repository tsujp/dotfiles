#!/usr/bin/env bash

# Using Homebrew.
eval "$(/opt/homebrew/bin/brew shellenv)"

# Homebrew .NET.
export DOTNET_ROOT="/opt/homebrew/opt/dotnet/libexec"
export PATH="\
$PATH:\
/opt/homebrew/bin/dotnet:\
$HOME/.azure/tools\
"

# Homebrew gpg version 2 executable is just `gpg`.
alias gpg='gpg'
