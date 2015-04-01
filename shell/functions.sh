# shell/functions.sh

# Fuzzy-select a tmux session to reattach
function ts() {
  tmux attach-session -t $(tmux ls | sed 's/:.*//' | pick)
}

# Fuzzy-select a process to kill
function killp() {
  kill $(ps -e | awk '{if(NR!=1) { print $4, $1  }}' | pick -do | tail -n +2)
}

# Fuzzy-select ruby version using rbenv
function chr() {
  if [[ $1 =~ '^(shell|local|global)$' ]]; then
    rbenv "$1" $(rbenv versions | sed -rn 's/[\* ]? ([[:alnum:]\.\-]+).*/\1/p' | pick)
    echo "Using Ruby version $(rbenv $1)"
  else
    echo 'Usage: chr (shell|local|global)'
  fi
}

# create dir $1 and cd into it, creating subdirectories as necessary
function mcd() {
  mkdir -p "$1" && cd "$1";
}

# search PWD and subdirectories for $1
function find_file() {
  ls **/*$1*
}

# pretty-print the command search path
function p() {
  if [[ $1 == 'path' ]]; then
    ruby -e 'puts `echo $PATH`.gsub(":", "\n")'
  elif [[ $1 == 'manpath' ]]; then
    ruby -e 'puts `echo $MANPATH`.gsub(":", "\n")'
  fi
}

# update homebrew, upgrade packages, cleanup
function update_homebrew() {
  echo brew update  && brew update
  echo brew upgrade && brew upgrade
  echo brew cleanup && brew cleanup
  echo brew doctor  && brew doctor
}


# rebase non-master branches of dotfiles onto master and force push
function update_dotfiles () {
  for branch in $(git branch | awk -F* '{ print $1  }'); do
    git rebase master $branch
  done

  git checkout master
  git push origin --all --force
}

# Mac-specific: show and hide the desktop
function desktop() {
  if [[ $1 != "hide" && $1 != "show" ]]; then
    echo "Usage: desktop [show|hide]"; return 1;
  fi
  if [[ $1 == "hide" ]]; then
    local view=false;
  elif [[ $1 == "show" ]]; then
    local view=true;
  fi
  defaults write com.apple.finder CreateDesktop -bool $view;
  killall Finder;
}

# Mac-specific: show and hide hidden files
function hidden_files() {
  if [[ $1 != "hide" && $1 != "show" ]]; then
    echo "Usage: hidden_files [show|hide]"; return 1;
  fi
  if [[ $1 == "hide" ]]; then
    local view=false;
  elif [[ $1 == "show" ]]; then
    local view=true;
  fi
  defaults write com.apple.finder AppleShowAllFiles $view;
  killall Finder;
}

# wrap rm -rf with a guard prompt
function delete() {
  echo "rm -rf $@"
  echo -n "rm: Permanently delete the selected files/directories? "
  echo -n "This cannot be undone. "
  read dir

  if [[ $dir =~ ^[yY](es)?$ ]]; then
    echo "Deleting: $@"
    rm -rf "$@"
  else
    echo "Canceling with no changes made."
  fi
}

# generate a playground project to test out a feature, library, etc.
# usage: play FRAMEWORK PROJECT-NAME
function play () {
  if [[ $1 =~ ^rails$ ]]; then
    rplay -n $2 --skip-bundle
  elif [[ $1 =~ ^(express|play|flask|jade|om|spark)$ ]]; then
    echo "$1 playgrounds haven't been set up yet"
  else
    echo 'Usage example: play rails test_project'
  fi
}
