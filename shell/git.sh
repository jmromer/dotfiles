# shell/git.sh
#-------------------------------------------------------------
# ALIASES
#-------------------------------------------------------------
alias git='hub' # use hub for github-flavored git

#-------------------------------------------------------------
# FUNCTIONS
#-------------------------------------------------------------
function clone() {
  git clone $1 $2         # git@github.com:user/repo_name.git
  repo=${1##*/}           # repo_name.git
  dir=${2:=${repo%.*}}    # repo_name unless $2 is provided
  cd $dir                 # cds into repo directory
}

# No arguments: `git status`
# With arguments: acts like `git`
function g() {
  if [[ $# > 0 ]]; then
    git $@
  else
    \git status -sb
  fi
}

#-------------------------------------------------------------
# COLORIZED GIT PROMPT
#-------------------------------------------------------------
function git_color() {
  local clean='working (directory|tree) clean'

  if [[ $git_status =~ $clean ]]; then
    if [[ $git_status =~ "Your branch is ahead of" ]]; then
      echo -ne $(color yellow)
    else
      echo -ne $(color green)
    fi
  elif [[ $git_status =~ "Unmerged" ]]; then
    echo -ne $(color violet)
  else
    echo -ne $(color red)
  fi
}

function git_branch() {
  local git_status="$(\git status 2> /dev/null)"
  local is_on_branch='^On branch ([^[:space:]]+)'
  local is_on_commit='HEAD detached at ([^[:space:]]+)'
  local is_rebasing="rebasing branch '([^[:space:]]+)' on '([^[:space:]]+)'"

  if [[ $git_status =~ $is_on_branch ]]; then
    local branch=${BASH_REMATCH[1]:-$match[1]} # bash/zsh portable
    if [[ $git_status =~ "Unmerged paths" ]]; then
      git_color && echo -n "merging into $branch "
    else
      git_color && echo -n "$branch "
    fi
  elif [[ $git_status =~ $is_on_commit ]]; then
    local commit=${BASH_REMATCH[1]:-$match[1]}
    git_color && echo -n "$commit "
  elif [[ $git_status =~ $is_rebasing ]]; then
    local branch=${BASH_REMATCH[1]:-$match[1]}
    local commit=${BASH_REMATCH[2]:-$match[2]}
    git_color && echo -n "rebasing $branch on $commit "
  fi
}
