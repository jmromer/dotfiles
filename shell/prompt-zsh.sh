# shell/prompt-zsh.sh
#-------------------------------------------------------------
# ZSH GENERAL
#-------------------------------------------------------------
alias reload!="source ~/.zshrc"
setopt extendedglob # Enable extended globbing
unsetopt nomatch    # Allow [ or ] wherever you want
autoload -U zmv     # rename files like zmv '(*).txt' '$1.html'

#-------------------------------------------------------------
# COMMAND COMPLETION
#-------------------------------------------------------------
shared='/usr/local/share'
fpath=(
  $HOME/.zsh/completion
  $shared/zsh-completions
  $shared/zsh/site-functions
  $SHELL_CONFIG/completions
  $fpath
)

autoload -U compinit && compinit
source $shared/zsh/site-functions/_aws
source $shared/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

compdef git=hub
compdef g=git

#-------------------------------------------------------------
# DIRECTORY STACK  (see http://j.mp/1lOiWio)
#-------------------------------------------------------------
setopt autocd autopushd pushd_minus pushd_silent
setopt pushd_to_home cdable_vars pushd_ignore_dups
DIRSTACKSIZE=10

#-------------------------------------------------------------
# HISTORY SETTINGS
#-------------------------------------------------------------
setopt hist_ignore_all_dups inc_append_history
HISTFILE=$HOME/.zhistory
HISTSIZE=4096
SAVEHIST=4096

#-------------------------------------------------------------
# COMMAND-LINE AND HISTORY NAVIGATION
#-------------------------------------------------------------
bindkey -M viins '^a' beginning-of-line
bindkey -M viins '^e' end-of-line
bindkey -M viins '^b' backward-char
bindkey -M viins '^f' forward-char
bindkey -M viins '^d' delete-char

bindkey -M viins '^k' kill-line

# issue the command, but keep it at the prompt
bindkey -M viins '^y' accept-and-hold
bindkey -M viins '^o' push-line-or-edit

# ^t Invoke FZF file finder
bindkey '^t' fzf-file-widget

bindkey "^[[3"  prefix-2     # ensure delete backwards deletes
bindkey "^[[3~" delete-char  # ensure delete forwards deletes

# Position cursor after ARG[0] (for argument/flag entry)
function after-first-word() {
  zle beginning-of-line
  zle forward-word
}
zle -N after-first-word
bindkey '^x' after-first-word

#-------------------------------------------------------------
# COMMAND-LINE HISTORY SEARCHING AND NAVIGATION
#-------------------------------------------------------------
bindkey -M viins '^r' history-incremental-search-backward
bindkey -M vicmd '^r' history-incremental-search-backward

autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

bindkey '\e[A' up-line-or-beginning-search
bindkey '\e[B' down-line-or-beginning-search
bindkey -s '\eOA' '\e[A'
bindkey -s '\eOB' '\e[B'

bindkey -M viins '^p' up-line-or-beginning-search
bindkey -M vicmd '^p' up-line-or-beginning-search
bindkey -M viins '^n' down-line-or-beginning-search
bindkey -M vicmd '^n' down-line-or-beginning-search

# back-i-search begins with current word
setopt complete_in_word

#-------------------------------------------------------------
# COLORS
#-------------------------------------------------------------
autoload -U colors && colors

function color {
  if   [[ $1 == 'red'    ]]; then echo -n %{$fg_no_bold[red]%}
  elif [[ $1 == 'yellow' ]]; then echo -n %{$fg_no_bold[yellow]%}
  elif [[ $1 == 'green'  ]]; then echo -n %{$fg_no_bold[green]%}
  elif [[ $1 == 'violet' ]]; then echo -n %{$fg_no_bold[magenta]%}
  elif [[ $1 == 'blue'   ]]; then echo -n %{$fg_no_bold[blue]%}
  elif [[ $1 == 'white'  ]]; then echo -n %{$fg_no_bold[white]%}
  elif [[ $1 == 'reset'  ]]; then echo -n %{$reset_color%}
  fi
}

#-------------------------------------------------------------
# PROMPT WITH SHORT PWD, COLORIZED GIT INFO
#-------------------------------------------------------------
setopt prompt_subst       # enables command substitution

PS1=$'\n$(color blue)%c ' # basename of pwd after a newline
PS1+='$(git_branch)'      # current branch or commit name, with color
PS1+='$(color reset)%# '  # reset color, add %
export PS1

#-------------------------------------------------------------
# Use Vim mode in Zsh
#-------------------------------------------------------------
bindkey -v
export KEYTIMEOUT=1      # no lag after pressing ESC to enter normal mode
setopt transient_rprompt # so modes for previous lines aren't displayed

# ------------ Right-side prompt -----------------------------

if [[ $TERM != 'eterm-color' ]]; then
  vim_ins_mode='[insert]'
  vim_cmd_mode='[normal]'
  vim_mode=$vim_ins_mode

  function zle-keymap-select {
    vim_mode="${${KEYMAP/vicmd/${vim_cmd_mode}}/(main|viins)/${vim_ins_mode}}"
    zle reset-prompt
  }

  function zle-line-init {
    vim_mode=$vim_ins_mode
    zle reset-prompt
  }

  zle -N zle-keymap-select
  zle -N zle-line-init

  RPROMPT='$(color yellow)${vim_mode}$(color reset)'
  RPROMPT2='$(color yellow)${vim_mode}$(color reset)'
fi

#-------------------------------------------------------------
# UNDO (DISABLE ZSH DEFAULTS)
#-------------------------------------------------------------
disable r       # disable zsh's r

#-------------------------------------------------------------
# OPAM config
#-------------------------------------------------------------
# OPAM configuration
. $HOME/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

#-------------------------------------------------------------
# iTerm2
#-------------------------------------------------------------
if [[ -f $HOME/.iterm2_shell_integration.zsh ]]; then
  source $HOME/.iterm2_shell_integration.zsh
fi

# #-------------------------------------------------------------
# # ZSH CONFIG
# #-------------------------------------------------------------
# # The following lines were added by compinstall
#
# zstyle ':completion:*' auto-description 'specify: %d '
# zstyle ':completion:*' completer _expand _complete _ignored _match _correct _approximate _prefix
# zstyle ':completion:*' completions 1
# zstyle ':completion:*' expand suffix
# zstyle ':completion:*' file-sort name
# zstyle ':completion:*' format 'Completing %d:'
# zstyle ':completion:*' glob 1
# zstyle ':completion:*' group-name ''
# zstyle ':completion:*' insert-unambiguous true
# zstyle ':completion:*' list-colors ''
# zstyle ':completion:*' list-suffixes true
# zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}' '+r:|[._-]=** r:|=**' '+l:|=* r:|=*'
# zstyle ':completion:*' max-errors 1 numeric
# zstyle ':completion:*' menu select=long
# zstyle ':completion:*' original true
# zstyle ':completion:*' prompt '"Corrections: "'
# zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
# zstyle ':completion:*' squeeze-slashes true
# zstyle ':completion:*' substitute 1
# zstyle ':completion:*' verbose true
# zstyle :compinstall filename '/Volumes/jkrmr/.zshrc'
#
# autoload -Uz compinit
# compinit
# # End of lines added by compinstall
# # Lines configured by zsh-newuser-install
# HISTFILE=~/.histfile
# HISTSIZE=10000
# SAVEHIST=10000
# setopt appendhistory autocd extendedglob notify
# unsetopt beep
# bindkey -v
# # End of lines configured by zsh-newuser-install
