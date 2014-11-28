# shell/git.sh
#-------------------------------------------------------------
# ALIASES
#-------------------------------------------------------------
alias git='gh' # use gh for github-flavored git
alias gf='git-flow' # for when using git flow gem

#-------------------------------------------------------------
# FUNCTIONS
#-------------------------------------------------------------
function clone() {
  git clone $1 $2         # git@github.com:user/repo_name.git
  repo=${1##*/}           # repo_name.git
  dir=${2:=${repo%.*}}    # repo_name unless $2 is provided
  cd $dir                 # cds into repo directory
}

function git_restore_from() {
  local warn="$(color red) ---- WARNING ---- $(color reset)"
        warn+=" You are about to fetch from remote"
        warn+="$(color yellow) $1 $(color reset)"
        warn+="and overwrite local branch"
        warn+="$(color yellow) $2$(color reset). "
        warn+="This cannot be undone. Continue?  "
  echo -ne $warn

  read dir
  if [[ $dir == 'yes' ]]; then
    local mssg="$(color yellow) "
          mssg+="Restoring branch $(color white)$2$(color yellow) "
          mssg+="from last commit at remote "
          mssg+="$(color white)$1$(color yellow).$(color reset)\n"
    echo -e $mssg
    # Hard reset to the most recent remote commit
    git fetch --all
    git reset --hard $1/$2
  else
    echo "Canceling with no changes made."; return 1;
  fi
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
  if [[ $git_status =~ "working directory clean" ]]; then
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
