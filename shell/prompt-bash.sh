# shell/prompt-bash.sh
#-------------------------------------------------------------
# Source .bash_profile
#-------------------------------------------------------------
alias reload!="source ~/.bash_profile"

#-------------------------------------------------------------
# COMMAND COMPLETION
#-------------------------------------------------------------
if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi
source $SHELL_CONFIG/completions/git-completion.bash
complete -C aws_completer aws

#-------------------------------------------------------------
# COLORS  # NB: wrapping incorrectly in bash
#-------------------------------------------------------------
function color {
  if   [[ $1 == 'red'    ]]; then echo -ne '\e[0;31m'   # $(tput setaf 1)
  elif [[ $1 == 'yellow' ]]; then echo -ne '\e[1;33m'   # $(tput setaf 3)
  elif [[ $1 == 'green'  ]]; then echo -ne '\e[0;32m'   # $(tput setaf 2)
  elif [[ $1 == 'violet' ]]; then echo -ne '\e[0;35m'   # $(tput setaf 5)
  elif [[ $1 == 'blue'   ]]; then echo -ne '\e[1;34m'   # $(tput setaf 4)
  elif [[ $1 == 'white'  ]]; then echo -ne '\e[0;31m'   # $(tput setaf 7)
  elif [[ $1 == 'reset'  ]]; then echo -ne '\e[0m'      # $(tput sgr0)
  fi
}

#-------------------------------------------------------------
# PROMPT WITH SHORT PWD, COLORIZED GIT INFO
#-------------------------------------------------------------
PS1='\n$(color blue)\W '  # history #, basename of pwd
PS1+='$(git_branch)'      # prints current branch
PS1+='$(color reset)\$ '  # resets color and prints $ or #
export PS1

#-------------------------------------------------------------
# HISTORY COMPLETION
#-------------------------------------------------------------
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

#-------------------------------------------------------------
# Use Vim mode in Bash
#-------------------------------------------------------------
set -o vi

#-------------------------------------------------------------
# GNU LS (use -G for BSD)
#-------------------------------------------------------------
alias ls="ls --color"

