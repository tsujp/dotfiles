#!/usr/bin/env bash

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## SET UP SHOPT  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

[[ "$-" != *i* ]] && { return ; }  # if not interactive shell abort

# TODO: If Bash 5.2 then allow the shopt stuff.
# https://www.gnu.org/software/bash/manual/html_node/The-Set-Builtin.html
set -b                 # Report terminated background job exit code immediately.
shopt -s cdspell       # Autocorrect close cd typos.
shopt -s checkwinsize  # Uupdate LINES and COLUMNS after commands to current size.
shopt -s histappend    # Append to history; don't overwrite.
shopt -s checkhash
shopt -s checkjobs


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## GUARDS  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#[[ "$-" = *i* ]] || return     # if interactive shell, exit
# [[ "$-" != *i* ]] && { return ; }  # if not interactive shell abort
#[[ "$TERM" = dumb ]] && return # in emacs TRAMP, exit


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## START 'JOBS'  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# TODO: For these startup 'jobs' add a timeout so if they take more than.. 1 second I get an error. As it currently is will see stdout/err.

# fix potential GNUPG permissions
if command -v find &> /dev/null; then
    (find "$XDG_CONFIG_HOME"/gnupg -type f -exec chmod 600 {} \; &)
    (find "$XDG_CONFIG_HOME"/gnupg -type d -exec chmod 700 {} \; &)
fi

# Git limits environment variables in git config so this is done here.

# 1. Check un-aliased command is in path and strip all _irrelevant_ whitespace (e.g. somehow a
#    whitespace-only response is captured from `command`). Note that this DOES NOT break paths
#    which contain spaces, e.g. if the target binary is at `/home/foo/has spaces/` it WILL be
#    preserved correctly and pass these tests.
if ! gpg2_executable="$(command -v gpg2 2> /dev/null)" && [[ -z "${gpg2_executable//[[:space:]]}" ]]; then
  printf '(dotfiles) COULD NOT FIND `gpg2` EXECUTABLE IN PATH!\n'
fi

# `gpg2_executable` is now (probably) a valid path.

# 2. Check path represents an executable binary (that we also have permission to execute too).
if [[ -x "${gpg2_executable}" ]]; then
  (git config --file "$XDG_CONFIG_HOME/git/variable" gpg.program "$gpg2_executable" &)
else
  printf '(dotfiles) LACK OF PERMISSION OR `gpg2` AT PATH %s IS NOT EXECUTABLE\n' "$gpg2_executable"
fi

# TODO: check ~/.ssh/config only has permissions 600, else set it so


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## ALIASES - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

alias ..='cd ..'
alias ...='cd ../..'
# TODO: make .. a function which takes numeric args to go up N levels

alias mv='mv -i -v'
# alias cd=__cdenv

alias ee='exit'
alias c='clear'
alias h='history'
alias hh='history | less +G'
# history | tail -30
alias rm='rm -i'
alias r=_reload_shell
alias k=kubectl

alias e_bc="$EDITOR $XDG_CONFIG_HOME/shell-profiles/base.bashrc"
alias e_my_bc="$EDITOR $XDG_CONFIG_HOME/shell-profiles/${HOSTNAME}-${USER}.bashrc"

alias e_bp="$EDITOR $XDG_CONFIG_HOME/shell-profiles/base.profile"
alias e_my_bp="$EDITOR $XDG_CONFIG_HOME/shell-profiles/${HOSTNAME}-${USER}.profile"

alias e_foot="$EDITOR $XDG_CONFIG_HOME/foot/foot.ini"
alias e_ssh="$EDITOR ~/.ssh/config"

alias e_gc="$EDITOR ${XDG_CONFIG_HOME}/git/config"

#alias ll='ls  -lFhN    --color --group-directories-first'
#alias lla='ls -lFhN -A --color --group-directories-first'
#alias l='ls  -FhN     --color --group-directories-first'
#alias ls='ls  -FhN     --color --group-directories-first'
#alias la='ls  -FhN  -A --color --group-directories-first'

alias recent='ls -ltch --color'
alias py='python3'
alias gnpm='npm list -g --depth 0'
alias dpsv='docker ps -a | less -S'

alias gb='git branch'
alias g='git'

alias dots='git --work-tree=$HOME --git-dir=$HOME/.dotfiles.git'
alias dots-all='dots status --ignored=matching'

alias purge-dsstore="find . -name .DS_Store -type f -delete -print"

#alias gpg='gpg2'
alias gpg-restart='gpg-connect-agent reloadagent /bye'
alias rsync='rsync -I --info=progress2'
alias wget="wget --hsts-file=$XDG_CACHE_HOME/wget-hsts"
alias off='sudo shutdown -P now'
alias grepi='grep -i'

# TODO: Put this in bin as these are 'static' helpers
alias ppath=pretty_print_path
alias chk-gpg=_check_gnupg_working

# TODO: put this in tvoid's own profile
#alias xbpsi='sudo xbps-install -Su'
#alias xbpsq='xbps-query -Rs'

# COMPLETIONS
# source <(kubectl completion bash)
# complete -F __start_kubectl k


## - - - - - - - - - - - - - - - - - -
## FUNCTION ALIASES  - - - - - - - - -

# TODO: Replace tput with escape codes.
man ()
{
  # TODO: replace tput with escape codes for colour
	env \
	LESS_TERMCAP_mb=$'\033[1;36m' \
	LESS_TERMCAP_md=$'\033[1;36m' \
	LESS_TERMCAP_me=$(tput sgr0) \
	LESS_TERMCAP_se=$(tput rmso; tput sgr0) \
	LESS_TERMCAP_ue=$(tput rmul; tput sgr0) \
	LESS_TERMCAP_us=$'\033[1;4;34m' \
	LESS_TERMCAP_mr=$'\033[7m' \
	LESS_TERMCAP_mh=$'\033[2m' \
	LESS_TERMCAP_ZN=$(tput ssubm) \
	LESS_TERMCAP_ZV=$(tput rsubm) \
	LESS_TERMCAP_ZO=$(tput ssupm) \
	LESS_TERMCAP_ZW=$(tput rsupm) \
		man "$@"
}

# TODO: Shell alias to list these helpers.
bundle_init ()
{
  # Want local Gem installation
  bundle init
  mkdir -p .bundle
  tee .bundle/config << EOF > /dev/null
---
BUNDLE_PATH: 'vendor/bundle'
EOF
}

# TODO: Should remove global_ignore_base prefixed entries from dots_untracked_list and display the result.
# dotties ()
# {
#     pushd "$(pwd)"
#     cd "$HOME"
#     local -r global_ignore_base="$(dots check-ignore *)"
#     local -r dots_untracked_list="$(dots-all | sed '1,/Ignored files/d')"

#     awk -v abc="$global_ignore_base" '
#         { print $abc; }
#     '
# }


## - - - - - - - - - - - - - - - - - -
## FUNCTION HELPERS  - - - - - - - - -

_reload_shell ()
{
  #puts_section 'Reloading shell configuration'
  hash -r && _SHOW_MESSAGES=1 exec -a -bash bash
}

_check_gnupg_working ()
{
  printf 'foo bar\n' | gpg --clearsign
}


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# * * * * * * * * * COLOURS

# FILES AND DIRECTORIES
# TODO: customise this further?
# TODO: make a generator script for this?
export LS_COLORS="rs=0:fi=0:di=1;34:ln=95:mh=30;46:pi=40;38;5;11:so=95:do=95:bd=48;5;232;38;5;11:cd=48;5;232;38;5;3:or=95;40:mi=05;48;5;232;38;5;15:su=48;5;196;38;5;15:sg=48;5;11;38;5;16:ca=48;5;196;38;5;226:tw=48;5;10;38;5;16:ow=48;5;10;38;5;21:st=48;5;21;38;5;15:ex=93:*.tar=31:*.tgz=31:*.arc=31:*.arj=31:*.taz=31:*.lha=31:*.lz4=31:*.lzh=31:*.lzma=31:*.tlz=31:*.txz=31:*.tzo=31:*.t7z=31:*.zip=31:*.z=31:*.Z=31:*.dz=31:*.gz=31:*.lrz=31:*.lz=31:*.lzo=31:*.xz=31:*.bz2=31:*.bz=31:*.tbz=31:*.tbz2=31:*.tz=31:*.deb=31:*.rpm=31:*.jar=31:*.war=31:*.ear=31:*.sar=31:*.rar=31:*.alz=31:*.ace=31:*.zoo=31:*.cpio=31:*.7z=31:*.rz=31:*.cab=31:*.jpg=95:*.jpeg=95:*.gif=95:*.bmp=95:*.pbm=95:*.pgm=95:*.ppm=95:*.tga=95:*.xbm=95:*.xpm=95:*.tif=95:*.tiff=95:*.png=95:*.svg=95:*.svgz=95:*.mng=95:*.pcx=95:*.mov=95:*.mpg=95:*.mpeg=95:*.m2v=95:*.mkv=95:*.webm=95:*.ogm=95:*.mp4=95:*.m4v=95:*.mp4v=95:*.vob=95:*.qt=95:*.nuv=95:*.wmv=95:*.asf=95:*.rm=95:*.rmvb=95:*.flc=95:*.avi=95:*.fli=95:*.flv=95:*.gl=95:*.dl=95:*.xcf=95:*.xwd=95:*.yuv=95:*.cgm=95:*.emf=95:*.axv=95:*.anx=95:*.ogv=95:*.ogx=95:*.aac=36:*.au=36:*.flac=36:*.mid=36:*.midi=36:*.mka=36:*.mp3=36:*.mpc=36:*.ogg=36:*.ra=36:*.wav=36:*.axa=36:*.oga=36:*.spx=36:*.xspf=36"



# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# * * * * * * * * * PROMPT CONSTRUCTION
# TODO: Custom prompt in Zig with some cool logic stuff.
# INPUTRC COMMANDS WE'RE SETTING FROM BASHRC FOR CONVENIENCE
# https://www.gnu.org/software/bash/manual/bash.html#Readline-Init-File-Syntax

shell_depth=""
if [[ $SHLVL -gt 1 ]]; then
   shell_depth="($SHLVL) "
fi

# Disable for now (2025/02/24: messing around with sparse-checkout worktrees and __git_prompt logic is messing with that)
# PROMPT_COMMAND="__git_prompt;"
PROMPT_COMMAND="__temp_git_replace;"
PS1_PREFIX="\[\033[36m\]\w\[\033[m\] "
PS1_SUFFIX="\[\033[33m\]\$\[\033[m\] "

__temp_git_replace ()
{
    printf -v PS1 -- '%s' "$shell_depth$PS1_PREFIX$PS1_SUFFIX"
}


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# * * * * * * * * * GIT PROMPT INTERNAL AND HELPER FUNCTION
# TODO: From zig using git api instead (faster).

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## NOTES - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# .profile, .bash_profile, .bashrc et al
#   - .profile       environment variables, PATH, must be (mostly) shell agnostic
#   - .bash_profile  for login shells (macOS always a login shell), aliases, ...
#   - .bashrc        new instances of bash not from login e.g. typing /bin/bash
#   - .bash_login    ensure this file never exists as it causes others to be ignored
#
#  A, B, C means executes A then B then C
#  B1, B2, B3 means it will only execute a single one (first found)
#
#  So, if ~/.profile found first (B3) then B2 and B1 never execute, profile
#  could itself call say ~/.bashrc though
#
#            +----------------+-----------+-----------+------+
#            |                |Interactive|Interactive|Script|
#            |                |login      |non-login  |      |
#            +----------------+-----------+-----------+------+
#            |/etc/profile    |   A       |           |      |
#            +----------------+-----------+-----------+------+
#            |/etc/bash.bashrc|           |    A      |      |
#            +----------------+-----------+-----------+------+
#            |~/.bashrc       |           |    B      |      |
#            +----------------+-----------+-----------+------+
#            |~/.bash_profile |   B1      |           |      |
#            +----------------+-----------+-----------+------+
#            |~/.bash_login   |   B2      |           |      |
#            +----------------+-----------+-----------+------+
#            |~/.profile      |   B3      |           |      |
#            +----------------+-----------+-----------+------+
#            |BASH_ENV        |           |           |  A   |
#            +----------------+-----------+-----------+------+
#            |~/.bash_logout  |    C      |           |      |
#            +----------------+-----------+-----------+------+
