#!/usr/bin/env bash

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## SET UP SHOPT  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# https://www.gnu.org/software/bash/manual/html_node/The-Set-Builtin.html
set -b                 # report terminated background job exit code immediately
shopt -s cdspell       # autocorrect close cd typos
shopt -s checkwinsize  # update LINES and COLUMNS after commands to current size
shopt -s histappend    # append to history don't overwrite


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## SOURCE MORE - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# asdf version manager, and its shell completion
[[ -e "$HOME/bin/asdf/asdf.sh" ]] && source "$HOME/bin/asdf/asdf.sh"
[[ -e "$HOME/bin/asdf/completions/asdf.bash" ]] && {
  source "$HOME/bin/asdf/completions/asdf.bash"
}


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## GUARDS  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

[[ "$-" = *i* ]] || return     # if interactive shell, exit
[[ "$TERM" = dumb ]] && return # in emacs TRAMP, exit


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## START 'JOBS'  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# fix potential GNUPG permissions
if command -v find &> /dev/null; then
  find "$XDG_CONFIG_HOME"/gnupg -type f -exec chmod 600 {} \;
  find "$XDG_CONFIG_HOME"/gnupg -type d -exec chmod 700 {} \;
fi

# git; limited environment variables in git config so this is done here
# git config --global core.excludesfile "$XDG_CONFIG_HOME/git/gitignore_global"
git config --file "$XDG_CONFIG_HOME/git/variable"  gpg.program $(which gpg2)

# TODO: check ~/.ssh/config only has permissions 600, else set it so


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## ALIASES - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias mv='mv --no-clobber' # do not overwrite existing files
# alias cd=__cdenv
alias ee='exit'
alias c='clear'
alias h='history | tail -30'
alias rm='rm -i'
alias r=_reload_shell
alias k=kubectl
alias e-bc="$EDITOR $XDG_CONFIG_HOME/shell-profiles/$(hostname)-$(whoami).bashrc"
alias e-bp="$EDITOR $XDG_CONFIG_HOME/shell-profiles/$(hostname)-$(whoami).profile"
alias e-foot="$EDITOR $XDG_CONFIG_HOME/foot/foot.ini"
alias e-ssh="$EDITOR ~/.ssh/config"
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
alias gpg='gpg2'
alias gpg-restart='gpg-connect-agent reloadagent /bye'
alias rsync='rsync -I --info=progress2'
alias wget="wget --hsts-file=$XDG_CACHE_HOME/wget-hsts"
alias off='sudo shutdown -P now'
alias grepi='grep -i'
alias ppath=pretty_print_path

# TODO: put this in it's own script which is included if voidlinux
alias xbpsi='sudo xbps-install -Su'
alias xbpsq='xbps-query -Rs'

# COMPLETIONS
# source <(kubectl completion bash)
# complete -F __start_kubectl k


## - - - - - - - - - - - - - - - - - - 
## FUNCTION ALIASES  - - - - - - - - -

man ()
{
  # TODO: replace tput with escape codes for colour
	env \
	LESS_TERMCAP_mb=$(tput bold; tput setaf 6) \
	LESS_TERMCAP_md=$(tput bold; tput setaf 6) \
	LESS_TERMCAP_me=$(tput sgr0) \
	LESS_TERMCAP_se=$(tput rmso; tput sgr0) \
	LESS_TERMCAP_ue=$(tput rmul; tput sgr0) \
	LESS_TERMCAP_us=$(tput smul; tput bold; tput setaf 4) \
	LESS_TERMCAP_mr=$(tput rev) \
	LESS_TERMCAP_mh=$(tput dim) \
	LESS_TERMCAP_ZN=$(tput ssubm) \
	LESS_TERMCAP_ZV=$(tput rsubm) \
	LESS_TERMCAP_ZO=$(tput ssupm) \
	LESS_TERMCAP_ZW=$(tput rsupm) \
		man "$@"
}


## - - - - - - - - - - - - - - - - - - 
## FUNCTION HELPERS  - - - - - - - - -

_reload_shell ()
{
  #puts_section 'Reloading shell configuration'
  hash -r && _SHOW_MESSAGES=1 exec -a "$0" bash
}

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# * * * * * * * * * FUNCTIONS AS HELPERS


__git_guard ()
{
  git -C . rev-parse
}

__help_hook ()
{
  if [[ "$1" = --help ]]; then
    __print_help "$2" "$3"
    return 1
  else
    return 0
  fi
}

__print_help ()
{
  COLUMNS=$(tput cols)
  title="USER MANUAL ($1)"
  printf "%*s\n" $((($(printf "%s" "$title" | wc -c ) + 80) / 2)) "$title"
  printf "%s" "$2"
}

__print_error ()
{
  printf "$(tput setaf 1)error $(tput setaf 2)%s $(tput sgr0)%s\n" "$1" "$2"
}



# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# * * * * * * * * * FUNCTIONS SERVING AS ALIASES



# CREATE NEW GITHUB BRANCH, OPTIONALLY ON THE GIVEN REMOTE TOO
gnb ()
{
  # function name
  local name="${FUNCNAME[0]}"

  # help text
  local help="
NAME
      ${FUNCNAME[0]} - Git New Branch

SYNOPSIS
      ${FUNCNAME[0]} [FLAGS] BRANCH

DESCRIPTION
      Creates a new git branch for you and also optionally updates the remote
      with said branch.

OPERANDS
      BRANCH   git compliant branch name

FLAGS
      -r       if present also pushes branch to remote.
"

  __help_hook "$1" "$name" "$help" || return 1
  __git_guard || return 1

  # check required args
  if [[ "$#" -lt 1 ]]; then
    __print_error "$name" "missing BRANCH operand" && return 1
  fi

  # OPTIND required for predictable behaviour
  local OPTIND remote

  # default to $1 for branch, if -r is set it will override
  local branch="$1"

  while getopts ":r:" flag; do
    case "$flag" in
      r)
        remote=1
        branch="$OPTARG"
        ;;
      \?)
        __print_error "-$OPTARG" "is not a valid flag, see --help" && return 1
        break
        ;;
      :)
        __print_error "-$OPTARG" "requires a BRANCH operand" && return 1
        break
        ;;
    esac
  done
  shift $((OPTIND-1))

  git checkout -b "$branch"

  if [[ "$?" != "0" ]]; then
    __print_error "$branch" "already exists, stopping execution" && return 1
  fi

  if ((remote)); then
    git push --set-upstream origin "$branch"
  fi
}



# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# * * * * * * * * * COLOURS
declare -A __c=([BOLD]="$(tput bold)" [RESET]="$(tput sgr0)")

# CONSTRUCT COLOURS
if [ "$(tput colors)" -ge 8 ]; then
  __c+=(
    [RED]="$(tput setaf 1)"     [BOLD_RED]=${__c[BOLD]}${__c[RED]}
    [GREEN]="$(tput setaf 2)"   [BOLD_GREEN]=${__c[BOLD]}${__c[GREEN]}
    [YELLOW]="$(tput setaf 3)"  [BOLD_YELLOW]=${__c[BOLD]}${__c[YELLOW]}
    [BLUE]="$(tput setaf 4)"    [BOLD_BLUE]=${__c[BOLD]}${__c[BLUE]}
    [MAGENTA]="$(tput setaf 5)" [BOLD_MAGENTA]=${__c[BOLD]}${__c[MAGENTA]}
    [CYAN]="$(tput setaf 6)"    [BOLD__cYAN]=${__c[BOLD]}${__c[CYAN]}
    [WHITE]="$(tput setaf 7)"   [BOLD_WHITE]=${__c[BOLD]}${__c[WHITE]}
  )
fi

# HELPER FUNCTION
__uc ()
{
  local return_colour

  case "$#" in
    1)  return_colour="\[${__c[$1]}\]"
    ;;
    *)  echo "incorrect argument count passed to __uc"
    ;;
  esac

  # TODO confirm that a valid colour was selected

  echo "$return_colour"
}

# FILES AND DIRECTORIES
# todo customise this further?
# make a generator script for this?
export LS_COLORS="rs=0:fi=0:di=1;34:ln=95:mh=30;46:pi=40;38;5;11:so=95:do=95:bd=48;5;232;38;5;11:cd=48;5;232;38;5;3:or=95;40:mi=05;48;5;232;38;5;15:su=48;5;196;38;5;15:sg=48;5;11;38;5;16:ca=48;5;196;38;5;226:tw=48;5;10;38;5;16:ow=48;5;10;38;5;21:st=48;5;21;38;5;15:ex=93:*.tar=31:*.tgz=31:*.arc=31:*.arj=31:*.taz=31:*.lha=31:*.lz4=31:*.lzh=31:*.lzma=31:*.tlz=31:*.txz=31:*.tzo=31:*.t7z=31:*.zip=31:*.z=31:*.Z=31:*.dz=31:*.gz=31:*.lrz=31:*.lz=31:*.lzo=31:*.xz=31:*.bz2=31:*.bz=31:*.tbz=31:*.tbz2=31:*.tz=31:*.deb=31:*.rpm=31:*.jar=31:*.war=31:*.ear=31:*.sar=31:*.rar=31:*.alz=31:*.ace=31:*.zoo=31:*.cpio=31:*.7z=31:*.rz=31:*.cab=31:*.jpg=95:*.jpeg=95:*.gif=95:*.bmp=95:*.pbm=95:*.pgm=95:*.ppm=95:*.tga=95:*.xbm=95:*.xpm=95:*.tif=95:*.tiff=95:*.png=95:*.svg=95:*.svgz=95:*.mng=95:*.pcx=95:*.mov=95:*.mpg=95:*.mpeg=95:*.m2v=95:*.mkv=95:*.webm=95:*.ogm=95:*.mp4=95:*.m4v=95:*.mp4v=95:*.vob=95:*.qt=95:*.nuv=95:*.wmv=95:*.asf=95:*.rm=95:*.rmvb=95:*.flc=95:*.avi=95:*.fli=95:*.flv=95:*.gl=95:*.dl=95:*.xcf=95:*.xwd=95:*.yuv=95:*.cgm=95:*.emf=95:*.axv=95:*.anx=95:*.ogv=95:*.ogx=95:*.aac=36:*.au=36:*.flac=36:*.mid=36:*.midi=36:*.mka=36:*.mp3=36:*.mpc=36:*.ogg=36:*.ra=36:*.wav=36:*.axa=36:*.oga=36:*.spx=36:*.xspf=36"



# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# * * * * * * * * * PROMPT CONSTRUCTION
# INPUTRC COMMANDS WE'RE SETTING FROM BASHRC FOR CONVENIENCE
bind 'set editing-mode vi'
bind 'set show-mode-in-prompt on'
bind 'set keyseq-timeout 50'
bind 'set vi-ins-mode-string "\1\e[0m\2(i)"'
bind 'set vi-cmd-mode-string "\1\e[32m\2(c)\1\e[0m\2"'

shell_prompt_decoration="# "
shell_depth=""
if command -v pstree &> /dev/null; then
  depth="$(pstree -s "$$" | grep bash- -o | tail +2 | wc -l)"
  for (( i = 0; i < depth; i++ )); do
    shell_depth+="$shell_prompt_decoration"
  done
else
  # pstree not found, so output a single shell_prompt_decoration
  shell_depth="$shell_prompt_decoration"
fi

PROMPT_COMMAND="__git_prompt;"
PS1_PREFIX=" $(__uc CYAN)\W$(__uc RESET) "
PS1_SUFFIX="$(__uc YELLOW)$shell_depth$(__uc RESET)"



# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# * * * * * * * * * GIT PROMPT INTERNAL AND HELPER FUNCTION
__git_prompt ()
{
  local inside_worktree="$(git rev-parse --is-inside-work-tree 2>/dev/null)"

  local bad_colour="$(__uc RED)"
  local ok_colour="$(__uc GREEN)"
  local c_clear="$(__uc RESET)"

  local w="" # unstaged dirty files
  local i="" # staged dirty files
  local s="" # any stashed files
  local u=""
  local c=""
  local p=""
  local b="" # branch name

  local printf_format="%s" # overall output format
  local gitstring

  if [ "true" = "$inside_worktree" ]; then
    # force set true
    git config --bool bash.showDirtyState true

    # find staged files (dirty)
    git diff --no-ext-diff --quiet || w="*"
    git diff --no-ext-diff --cached --quiet || i="+"

    # find stash
    if git rev-parse --verify --quiet refs/stash >/dev/null; then
      s="$"
    fi

    # find git branch, based on http://stackoverflow.com/a/13003854/170413
    if b="$(git rev-parse --abbrev-ref HEAD 2> /dev/null)"; then
      if [ "$b" = "HEAD" ]; then
        b='detached*'
      else
        # hides branch name; remove if you want it shown
        b=''
      fi
    fi

    # set colours
    if [ "$w" = "*" ]; then
      w="$bad_colour$w"
    fi
    if [ -n "$i" ]; then
      i="$ok_colour$i"
    fi
    if [ -n "$s" ]; then
      s="$flags_colour$s"
    fi
    if [ -n "$u" ]; then
      u="$bad_colour$u"
    fi

    local file_info="$w$i$s$u"
    local branch_info="$c_clear$b"

    if [ -z "$b" ]; then
      gitstring="$file_info"
    else
      gitstring="$branch_info $file_info"
    fi

    gitstring="($gitstring$c_clear)"
  fi

  if [ -z "$gitstring" ]; then
    printf -v PS1 -- "$printf_format" "$PS1_PREFIX $PS1_SUFFIX"
  else
    printf -v PS1 -- "$printf_format" "$PS1_PREFIX$gitstring $PS1_SUFFIX"
  fi
}


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

