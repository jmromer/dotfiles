# shell/functions.sh

# create dir $1 and cd into it, creating subdirectories as necessary
function mcd() {
  mkdir -p "$1" && cd "$1";
}

# search PWD and subdirectories for $1
function find_file() {
  ls **/*$1*
}

# pretty-print the command search path
function path() {
  ruby -e 'puts `echo $PATH`.gsub(":", "\n")'
}

# pretty-print the manual search path
function pathman() {
  ruby -e 'puts `echo $MANPATH`.gsub(":", "\n")'
}

# remove brew package and all its dependencies
function brew_remove() {
  brew rm "$1"
  brew rm $(join <(brew leaves) <(brew deps "$1"))
}

# update homebrew, upgrade packages, cleanup
function update_homebrew() {
  echo brew update  && brew update
  echo brew upgrade && brew upgrade
  echo brew cleanup && brew cleanup
  echo brew doctor  && brew doctor
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

