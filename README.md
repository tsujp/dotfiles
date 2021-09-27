# Dotfiles

All my dotfiles, blah blah.

Includes a bunch of scripts under `meta-config/` to manage system-dependent
tweaks. That menu system is done via `sontek` which you can find
[here](https://github.com/tsujp/sontek).

## Install

Run `make` at the root of this repo and follow bash prompts for additional config.

```bash
git clone --recurse-submodules git@git.sr.ht:~tsujp/dotfiles .
```

## Configuration

### Bash

To get around the silliness of many different loadpaths under different scenarios like login vs non-login, whether or not each of these is interactive vs non-interactive all the shells I would use whilst at my computer are run as interactive login shells and the actual configuration of Bash is within `~/.bashrc`.

Excluding files in `/etc` which I do not set Bash loads configuration sources as follows (I do not consider further chain-loaded configurations as I wipe them):

```
+-----------+-----------------+-----------------+
|     .     |   Interactive   | Non-interactive |
+-----------+-----------------+-----------------+
| Login     | ~/.bash_profile | ~/.bash_profile |
| Non-Login | ~/.bashrc       | $BASH_ENV       |
+-----------+-----------------+-----------------+
```

#### Sane-ish Bash settings

So:

```
/home/tsujp
├── .profile         # generic/portable env vars and PATH
├── ~/.bash_profile  # symlinks to .bashrc in the same directory
└── ~/.bashrc        # calls .profile and sets host-specific .bashrc
```

