# shell/functions.sh

# restart homebrewed postgres
function restart-postgres() {
  launchctl unload ~/Library/LaunchAgents/homebrew.mxcl.postgresql.plist
  launchctl load ~/Library/LaunchAgents/homebrew.mxcl.postgresql.plist
}


function skim () {
  [ -z "$1" ]   && return 1 # arg not passed
  [ ! -e "$1" ] && return 1 # file doesn't exist
  [ -d "$1" ]   && return 1 # file a dir

  ~/Applications/Skim.app/Contents/MacOS/Skim "$1" &
}

# Display any processes listening on the given port
function listening_on_port() {
  lsof -wni tcp:$1
}

# Open the editor as appropriate
function e() {
  local editor=nvim

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

# Fuzzy-select a file from git status to open in editor
function eg() {
  local file="$(git select-files --no-multi)"

  if [[ ! -z "$file" ]]; then
    e $file
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

# Fuzzy-select ruby version using rbenv
function chr() {
  local scope="$(echo "shell\nlocal\nglobal" |\
    fzf-tmux -l 25 --no-sort --reverse --tiebreak=index)"

  local selected="$(rbenv versions |\
    sed -rn 's/[\* ]? ([[:alnum:]\.\-]+).*/\1/p' |\
    fzf-tmux -l 25 --no-sort --reverse --tiebreak=index)"

  echo "rbenv $scope $selected"
  rbenv $scope $selected
  rbenv version
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

# generate static ghost blog from currently running local instance
# wraps `buster generate` in order to copy files from `source` to the
# deploy directory
function buster_generate() {
  if [ -d static ]; then
    rm -rf ./static/*
    buster generate
    cp ./source/* ./static/
  else
    echo 'Wrong directory: Must be in ghost project root'
  fi
}

alias bgd='buster_generate && buster deploy'

# use binstub if available, else Bundler if bundled project, else whatever man
function bundle_or_bin() {
  if [ -e bin/$1 ]; then
    echo bin/$@
    bin/$@
  elif [ -f Gemfile ]; then
    echo bundle exec $@
    bundle exec $@
  else
    echo $@
    $@
  fi
}

alias s='bundle_or_bin rspec --format=progress'
alias ss='bundle_or_bin rspec --format=documentation'
alias ck='bundle_or_bin cucumber --format=progress'
alias ckk='bundle_or_bin cucumber'

alias rk='bundle_or_bin rake'
alias rg='bundle_or_bin rails generate'

alias rs='bundle_or_bin rails server'
alias rc='bundle_or_bin rails console'
alias rcs='bundle_or_bin rails console --sandbox'

alias rss='rk db:reset db:seed && rs'
