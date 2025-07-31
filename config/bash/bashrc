#!/usr/bin/env bash

# Bash startup load order
# -----------------------
#       /bin/bash - bash executable
#    /etc/profile - systemwide initialization file, executed for login shells
# ~/.bash_profile - user config, executed for login shells
#       ~/.bashrc - user per-interactive-shell startup file
#  ~/.bash_logout - user login shell cleanup file, executed when a login shell exits
#      ~/.inputrc - user readline initialization file

# shellcheck source=/dev/null
source "${HOME}/.dotfiles/env/setup.sh"

#-------------------------------------------------------------
# ALIASES
#-------------------------------------------------------------
alias bash="bash --init-file=${XDG_CONFIG_HOME}/bash/bashrc"

#-------------------------------------------------------------
# GIT PROMPT
#-------------------------------------------------------------
git_branch() {
  local git_status
  local is_on_branch
  local is_on_commit
  local is_rebasing

  git_status="$(\git status 2> /dev/null)"
  is_on_branch='^On branch ([^[:space:]]+)'
  is_on_commit='HEAD detached at ([^[:space:]]+)'
  is_rebasing="rebasing branch '([^[:space:]]+)' on '([^[:space:]]+)'"

  if [[ $git_status =~ $is_on_branch ]]; then
    local branch=${BASH_REMATCH[1]:-${match[1]}} # bash/zsh portable
    if [[ $git_status =~ "Unmerged paths" ]]; then
      printf "merging into $branch "
    else
      printf "$branch "
    fi
  elif [[ $git_status =~ $is_on_commit ]]; then
    local commit=${BASH_REMATCH[1]:-${match[1]}}
    printf "$commit "
  elif [[ $git_status =~ $is_rebasing ]]; then
    local branch=${BASH_REMATCH[1]:-${match[1]}}
    local commit=${BASH_REMATCH[2]:-${match[2]}}
    printf "rebasing $branch on $commit "
  fi
}

#-------------------------------------------------------------
# PROMPT WITH SHORT PWD, GIT INFO
#-------------------------------------------------------------
export PS1='\n\W $(git_branch)\$ '

#-------------------------------------------------------------
# HISTORY COMPLETION
#-------------------------------------------------------------
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'
