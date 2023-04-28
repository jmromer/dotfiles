# shellcheck disable=SC1090
# SC1090: Can't follow non-constant source.
#         Use a directive to specify location.

#-------------------------------------------------------------
# ALIASES: FILE MANAGEMENT, SHELL NAVIGATION
#-------------------------------------------------------------
alias ..='\cd ..; l'     # go to parent dir and list contents
alias ...='\cd ../..; l' # go to grandparent dir and list contents
alias mkdir='mkdir -p'   # create subdirectories as necessary
alias h='history'        # show history
alias d='dirs -v'        # show directory stack
alias ls='exa'
alias l='ls'
alias la='ls -a'
alias ld='ls -d .*'
alias ll='ls -l'
alias lla='ls -al'
alias lld='ls -al -d .*'
alias lt='exa --tree --level=3'

#-------------------------------------------------------------
# ALIASES: XDG dir navigation
#-------------------------------------------------------------
alias ccache="cd ${XDG_CACHE_HOME}"
alias cconfig="cd ${XDG_CONFIG_HOME}"
alias cdata="cd ${XDG_DATA_HOME}"
alias clocal="cd ${XDG_LOCALS_DIR}"
alias corg="cd ${ORG_HOME}"
alias crun="cd ${XDG_RUNTIME_DIR}"
alias cstate="cd ${XDG_STATE_HOME}"
alias cwork="cd ~/Work"

#-------------------------------------------------------------
# ALIASES: SAFEGUARDS
#-------------------------------------------------------------
alias rm='rm -i'  # confirm deletion
alias mv='mv -i'  # confirm move if overwriting existing file
alias cp='cp -i'  # confirm copy if overwriting existing file
alias ln='ln -iv' # display error if link exists; link verbosely

#-------------------------------------------------------------
# ALIASES: Postfix
#-------------------------------------------------------------
alias -g G='| grep --line-number --context=1' # grep w/ context
alias -g C="| pbcopy" # copy to clipboard
alias -g P='| less'   # send to pager

#-------------------------------------------------------------
# ALIASES: highlighting, xdg
#-------------------------------------------------------------
alias cat=bat
alias wget="wget --hsts-file=${XDG_CACHE_HOME}/wget/history"
alias bash="bash --init-file ${XDG_CONFIG_HOME}/bash/bashrc"

#-------------------------------------------------------------
# ALIASES: misc
#-------------------------------------------------------------
alias resign-xcode="sudo codesign -f -s ${USER} /Applications/Xcode.app"
alias pp='pretty-print-path'
alias b=bundle
alias vi='vim -U $XDG_CONFIG_HOME/vim/vimrc.minimal.vim'
alias vin='vim -U NONE'

#-------------------------------------------------------------
# EDITOR / PAGER
#-------------------------------------------------------------
export EDITOR="vim"
export PAGER="less"
export LESSOPEN="| src-hilite-lesspipe.sh %s"
export LESS=' --no-init --RAW-CONTROL-CHARS --quit-if-one-screen '


#-------------------------------------------------------------
# FUNCTIONS
#-------------------------------------------------------------
# create dir $1 and cd into it, creating subdirectories as necessary
function md() {
  local directory_name="${*// /-}"
  \mkdir -p "$directory_name"
  \cd "$directory_name" || return
}

function diff() {
    [[ -n "${1}" ]] && [[ -n "${2}" ]] || return
    "${HOMEBREW_PREFIX}/bin/diff" -u "${1}" "${2}" | delta
}

#-------------------------------------------------------------
# COLORIZED GIT PROMPT
#-------------------------------------------------------------
function git_color() {
  case "$git_status" in
    *'not staged'* |\
      *'to be committed'* |\
      *'untracked files present'* |\
      *'no rastreados'* |\
      *'archivos sin seguimiento'* |\
      *'a ser confirmados'*)
      echo -ne "$(color red)"
      ;;
    *'branch is ahead of'* |\
      *'have diverged'* |\
      *'rama está adelantada'* |\
      *'rama está detrás de'* |\
      *'han divergido'*)
      echo -ne "$(color yellow)"
      ;;
    *working\ *\ clean* |\
      *'está limpio'*)
      echo -ne "$(color green)"
      ;;
    *'Unmerged'* |\
      *'no fusionadas'* |\
      *'rebase interactivo en progreso'*)
      echo -ne "$(color violet)"
      ;;
    *)
      echo -ne "$(color white)"
      ;;
  esac
}

function git_branch() {
  git_status="$(\git status 2> /dev/null)"
  local is_on_branch='^(On branch|En la rama) ([^[:space:]]+)'
  local is_on_commit='HEAD (detached at|desacoplada en) ([^[:space:]]+)'
  local is_rebasing="(rebasing branch|rebase de la rama) '([^[:space:]]+)' (on|sobre) '([^[:space:]]+)'"
  local branch
  local commit

  if [[ $git_status =~ $is_on_branch ]]; then
    branch=${BASH_REMATCH[2]:-${match[2]}} # bash/zsh portable
    if [[ $git_status =~ "(Unmerged paths|no fusionadas)" ]]; then
      git_color && echo -n "merging into ${branch} "
    else
      git_color && echo -n "${branch} "
    fi
  elif [[ $git_status =~ $is_on_commit ]]; then
    commit=${BASH_REMATCH[2]:-${match[2]}}
    git_color && echo -n "${commit} "
  elif [[ $git_status =~ $is_rebasing ]]; then
    branch=${BASH_REMATCH[2]:-${match[2]}}
    commit=${BASH_REMATCH[4]:-${match[4]}}
    git_color && echo -n "rebasing ${branch} onto ${commit} "
  fi
}

# VCS_STATUS_COMMIT=c000eddcff0fb38df2d0137efe24d9d2d900f209
# VCS_STATUS_COMMITS_AHEAD=0
# VCS_STATUS_COMMITS_BEHIND=0
# VCS_STATUS_HAS_CONFLICTED=0
# VCS_STATUS_HAS_STAGED=0
# VCS_STATUS_HAS_UNSTAGED=1
# VCS_STATUS_HAS_UNTRACKED=1
# VCS_STATUS_NUM_ASSUME_UNCHANGED=0
# VCS_STATUS_NUM_CONFLICTED=0
# VCS_STATUS_NUM_STAGED=0
# VCS_STATUS_NUM_UNSTAGED=1
# VCS_STATUS_NUM_SKIP_WORKTREE=0
# VCS_STATUS_NUM_STAGED_NEW=0
# VCS_STATUS_NUM_STAGED_DELETED=0
# VCS_STATUS_NUM_UNSTAGED_DELETED=0
# VCS_STATUS_NUM_UNTRACKED=1
# VCS_STATUS_PUSH_COMMITS_AHEAD=0
# VCS_STATUS_PUSH_COMMITS_BEHIND=0
# VCS_STATUS_COMMIT_SUMMARY
function gitstatus_prompt() {
  PROMPT=$'\n'
  PROMPT+="$(color blue)%c "

  if gitstatus_query MY && [[ $VCS_STATUS_RESULT == ok-sync ]]; then
    if (( VCS_STATUS_HAS_CONFLICTED )); then
      PROMPT+="$(color violet)"
    elif (( VCS_STATUS_HAS_STAGED )) ||
         (( VCS_STATUS_HAS_UNSTAGED )) ||
         (( VCS_STATUS_HAS_UNTRACKED ));
    then
      PROMPT+="$(color red)"
    elif (( VCS_STATUS_COMMITS_AHEAD )) ||
         (( VCS_STATUS_COMMITS_BEHIND )) ||
         (( VCS_STATUS_PUSH_COMMITS_AHEAD )) ||
         (( VCS_STATUS_PUSH_COMMITS_BEHIND ));
    then
      PROMPT+="$(color yellow)"
    else
      PROMPT+="$(color green)"
    fi
    PROMPT+="${${VCS_STATUS_LOCAL_BRANCH:-@${VCS_STATUS_COMMIT:0:10}}//\%/%%} "  # escape %
  fi

  PROMPT+="$(color reset)%# "
  setopt no_prompt_{bang,subst} prompt_percent  # enable/disable correct prompt expansions
}

#-------------------------------------------------------------
# ZSH
#-------------------------------------------------------------
# GENERAL
#-------------------------------------------------------------
setopt extendedglob # Enable extended globbing
unsetopt nomatch    # Allow [ or ] wherever you want
autoload -U zmv     # rename files like zmv '(*).txt' '$1.html'


#-------------------------------------------------------------
# DIRECTORY STACK  (see http://j.mp/1lOiWio)
#-------------------------------------------------------------
setopt autocd autopushd pushd_minus pushd_silent
setopt pushd_to_home cdable_vars pushd_ignore_dups
export DIRSTACKSIZE=10

#-------------------------------------------------------------
# HISTORY SETTINGS
#-------------------------------------------------------------
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_SAVE_NO_DUPS
setopt INC_APPEND_HISTORY

export HISTORY_IGNORE="(ls|cd|pwd|exit|cd|h|l|lla|lld|g|g d|g co)"
export HISTFILE="${XDG_STATE_HOME}/zsh/history"
export HISTSIZE=1000
export SAVEHIST=1000

#-------------------------------------------------------------
# Zsh Plugins (load last)
#-------------------------------------------------------------

if [[ -z "$INSIDE_EMACS" ]]; then
    . "$XDG_DATA_HOME/zsh/zsh-vim-mode/zsh-vim-mode.plugin.zsh"
fi

. "$XDG_DATA_HOME/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

#-------------------------------------------------------------
# CURSOR POSITIONING
#-------------------------------------------------------------
# Position cursor after ARG[0] (for argument/flag entry)
function after-first-word() {
    zle beginning-of-line
    zle forward-word
}

zle -N after-first-word

#-------------------------------------------------------------
# vi / emacs modes
#-------------------------------------------------------------
if [[ -z "$INSIDE_EMACS" ]]; then
    # Change cursor shape for different vi modes.
    zle-keymap-select() {
        if [[ ${KEYMAP} == vicmd ]] ||
            [[ $1 = 'block' ]]; then
            echo -ne '\e[1 q'
        elif [[ ${KEYMAP} == main ]] ||
                [[ ${KEYMAP} == viins ]] ||
                [[ ${KEYMAP} = '' ]] ||
                [[ $1 = 'beam' ]]; then
            echo -ne '\e[5 q'
        fi
    }

    zle -N zle-keymap-select

    # no lag after pressing ESC to enter normal mode
    export KEYTIMEOUT=5

    # set vi keybindings
    bindkey -v

    echo -ne '\e[5 q' # Use beam shape cursor on startup.
    preexec() { echo -ne '\e[5 q'; } # Use beam shape cursor for each new prompt.
fi

#-------------------------------------------------------------
# HISTORY SEARCH
#-------------------------------------------------------------
# back-i-search should begin with the current word
setopt complete_in_word

autoload -U down-line-or-beginning-search
autoload -U up-line-or-beginning-search
zle -N down-line-or-beginning-search
zle -N up-line-or-beginning-search

#-------------------------------------------------------------
# Use ctrl-z as a toggle
#-------------------------------------------------------------

function ctrlz() {
    if [[ $#BUFFER -eq 0 ]]; then
        fg >/dev/null 2>&1 && zle redisplay
    else
        zle push-input
    fi
}

zle -N ctrlz

#-------------------------------------------------------------
# Use cd with pd
#-------------------------------------------------------------
function cd() {
    if type pd >/dev/null; then
      builtin cd "$(pd "$1")" || return
    else
      echo "warning: p/d not installed"
      builtin cd "$@" || return
    fi
}


#-------------------------------------------------------------
# Use FZF to set versions with ASDF
#-------------------------------------------------------------
function asdf-fzf() {
    BUFFER=$(asdf-select)
    zle end-of-line
}

zle -N asdf-fzf

#-------------------------------------------------------------
# KEY MAPS
#-------------------------------------------------------------
# zle defs

zle -N fzf-file-widget
zle -N prefix-2

# all modes

bindkey "^[[3"  prefix-2         # ensure delete backwards deletes
bindkey "^[[3~" delete-char      # ensure delete forwards deletes
bindkey "^\\"   asdf-fzf
bindkey "^t"    fzf-file-widget  # invoke FZF file finder
bindkey "^x"    after-first-word # move cursor to flag-insert position
bindkey "^z"    ctrlz

# string-based

bindkey -s "^h" "cd\n" # cd with pd
bindkey -s "^\]" "g cob\n" # change branches with pd

# emacs mode

bindkey -M emacs '^y' accept-and-hold
bindkey -M emacs '^o' push-line-or-edit

# vi insert mode

bindkey -M viins '^a' beginning-of-line
bindkey -M viins '^b' backward-char
bindkey -M viins '^d' delete-char
bindkey -M viins '^e' end-of-line
bindkey -M viins '^f' forward-char
bindkey -M viins '^k' kill-line
bindkey -M viins '^n' down-line-or-beginning-search
bindkey -M viins '^o' push-line-or-edit  # stash command, issue another, restore stash
bindkey -M viins '^p' up-line-or-beginning-search
bindkey -M viins '^r' history-incremental-search-backward
bindkey -M viins '^y' accept-and-hold    # issue the command, but keep it at the prompt

# vi command mode

bindkey -M vicmd '^n' down-line-or-beginning-search
bindkey -M vicmd '^p' up-line-or-beginning-search
bindkey -M vicmd '^r' history-incremental-search-backward

# arrow keys trigger history searching

bindkey '\e[A' up-line-or-beginning-search
bindkey '\e[B' down-line-or-beginning-search
bindkey -s '\eOA' '\e[A'
bindkey -s '\eOB' '\e[B'

#-------------------------------------------------------------
# COLORS
#-------------------------------------------------------------
autoload -U colors && colors

function color() {
    # shellcheck disable=SC2154
    [[ $1 == 'red'    ]] && printf "%s" "%{${fg_no_bold[red]}%}"
    [[ $1 == 'yellow' ]] && printf "%s" "%{${fg_no_bold[yellow]}%}"
    [[ $1 == 'green'  ]] && printf "%s" "%{${fg_no_bold[green]}%}"
    [[ $1 == 'violet' ]] && printf "%s" "%{${fg_no_bold[magenta]}%}"
    [[ $1 == 'blue'   ]] && printf "%s" "%{${fg_no_bold[blue]}%}"
    [[ $1 == 'white'  ]] && printf "%s" "%{${fg_no_bold[white]}%}"
    # shellcheck disable=SC2154
    [[ $1 == 'reset'  ]] && printf "%s" "%{${reset_color}%}"
}

#-------------------------------------------------------------
# PROMPT WITH SHORT PWD, COLORIZED GIT INFO
#-------------------------------------------------------------
setopt prompt_subst       # enables command substitution

if [[ -f "${HOMEBREW_PREFIX}/opt/gitstatus/gitstatus.plugin.zsh" ]]; then
    source "${HOMEBREW_PREFIX}/opt/gitstatus/gitstatus.plugin.zsh"
    gitstatus_stop 'MY' && gitstatus_start -s -1 -u -1 -c -1 -d -1 'MY'
    autoload -Uz add-zsh-hook
    add-zsh-hook precmd gitstatus_prompt
else
  PS1=$'\n$(color blue)%c ' # basename of pwd after a newline
  PS1+='$(git_branch)'      # current branch or commit name, with color
  PS1+='$(color reset)%# '  # reset color, add %
  export PS1
fi

#-------------------------------------------------------------
# COMMAND COMPLETION
#-------------------------------------------------------------
# shellcheck disable=SC2206
fpath=(
  ${BREW_PREFIX}/share/zsh/site-functions
  ${BREW_PREFIX}/share/zsh-completions
  ${ASDF_DIR}/completions
  ${ZDOTDIR}/completions
  $fpath
)

autoload -Uz compinit
compinit

compdef g=git


#-------------------------------------------------------------
# AUTOCOMPLETION
#-------------------------------------------------------------

if [[ -d "${XDG_DATA_HOME}/fzf/shell" ]]; then
  . "${XDG_DATA_HOME}/fzf/shell/completion.zsh"
  . "${XDG_DATA_HOME}/fzf/shell/key-bindings.zsh"
fi


#-------------------------------------------------------------
# SECURE / LOCALS
#-------------------------------------------------------------

[[ -f "${XDG_SECURE_DIR}/config/zsh/zshrc" ]] && \
    . "${XDG_SECURE_DIR}/config/zsh/zshrc"

[[ -f "${XDG_LOCALS_DIR}/config/zsh/zshrc" ]] && \
    . "${XDG_LOCALS_DIR}/config/zsh/zshrc"

[[ -f "${XDG_LOCALS_DIR}/config/zsh/.iterm2_shell_integration.zsh" ]] && \
    . "${XDG_LOCALS_DIR}/config/zsh/.iterm2_shell_integration.zsh"
