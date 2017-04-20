# shell/functions.sh

# restart homebrewed postgres
function restart_postgres() {
  launchctl unload ~/Library/LaunchAgents/homebrew.mxcl.postgresql.plist
  launchctl load ~/Library/LaunchAgents/homebrew.mxcl.postgresql.plist
}

# Display any processes listening on the given port
function listening_on_port() {
  lsof -wni tcp:$1
}

# Open the editor as appropriate
function e() {
  local editor=nvim

  if [ -n "$INSIDE_EMACS" ]; then
    echo emacsclient -n ${1:='.'}
    emacsclient -n ${1:='.'}
    return 0
  fi

  if [ -z "$1" ]; then
    echo $editor .
    $editor .
  elif [ "$#" -gt 1 ]; then
    $editor -p $@
  else
    echo $editor "$1"
    $editor "$1"
  fi
}
alias ee="emacs -nw"

# Fuzzy-select a file from git status to open in editor
function eg() {
  local file="$(git select-files)"

  if [[ ! -z "$file" ]]; then
    e $(echo "$file")
  fi
}

# Fuzzy-select a file from all those modified on the current git branch (up to
# origin/master) to open in editor
function egg() {
  local file="$(git branch-modified-files | fzf-tmux --reverse -d 20)"

  if [[ ! -z "$file" ]]; then
    e $(echo "$file")
  fi
}

# create dir $1 and cd into it, creating subdirectories as necessary
function md() {
  local directory_name=$(echo $@ | sed -e "s/\s/_/g")
  mkdir -p "$directory_name" && cd "$directory_name";
}

# pretty-print the command search path
function pp() {
  if [[ "$1" == 'path' ]]; then
    ruby -e 'puts `echo $PATH`.gsub(":", "\n")'
  elif [[ "$1" == 'manpath' ]]; then
    ruby -e 'puts `echo $MANPATH`.gsub(":", "\n")'
  elif [[ "$1" == 'cdpath' ]]; then
    ruby -e 'puts `echo $CDPATH`.gsub(":", "\n")'
  fi
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
function del() {
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
