# Dotfiles

All my dotfiles, system settings, and settings for various tools blah blah that
I use. This includes a bunch of scripts under `meta-config/` to manage
installation as well as change options once installed, e.g. to a different
font-scheme or from X to Wayland. That menu system is done via `sontek` which
you can find [here](https://github.com/tsujp/sontek).

## Install

Run `make` at the root of this repo and follow bash prompts.

```bash
git clone --recurse-submodules git@github.com:tsujp/dotfiles.git && cd $_ && make
```

### NB

This is tied intimately with Keybase which provides an e2e encrypted filesystem
which is where I store all of my sensitive files and tokens instead of using ppg
to encrypt files and putting them in Git; any problems with your use are likely 
due to those dependencies/files being missing. It's quite obvious which is which
and I have also marked them inline of `bashrc`.
