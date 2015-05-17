# shell/functions.sh

# curl for JSON
# usage: json METHOD PATH [params]
function json() {
  local format='accept:application/json'
  local method=$(echo $1 | awk '{print toupper($0)}')
  local url=http://localhost:3000/$2
  [[ ! -z $3 ]] && local params=${${3//: /=}//, /&}

  echo "curl -X $method -H $format $url $params"
  curl -X $method -H $format $url $params
}

# Display any processes listening on the given port
function listening_on_port() {
  lsof -wni tcp:$1
}

# Open the editor as appropriate
function e() {
  # local editor="$(ruby -e 'print %w(emacs vim).at(rand(2))')"
  local editor=vim

  if [ -z "$1" ]; then
    echo $editor .
    $editor .
  else
    echo $editor "$1"
    $editor "$1"
  fi
}


# Fuzzy-select a tmux session to reattach
function ts() {
  local session=$(tmux ls | sed 's/:.*//' | fzf)

  if [[ ! -z $session ]]; then
    echo "tmux attach-session -t $session"
    tmux attach-session -t $session
  fi
}

# Fuzzy-select a tmuxinator-managed tmux session
function mx() {
  local session=$(tmuxinator list | sed -n '1!p' | sed 's/\s\+/\n/g' | fzf)
  local subcommand=${1:='start'}

  if [[ ! -z $session ]]; then
    echo "tmuxinator $subcommand $session"
    tmuxinator $subcommand $session
  fi
}

# Fuzzy-select ruby version using rbenv
function chr() {
  if [[ $1 =~ '^(shell|local|global)$' ]]; then
    local version=$(rbenv versions | sed -rn 's/[\* ]? ([[:alnum:]\.\-]+).*/\1/p' | fzf)
    echo "rbenv $1 $version"
    rbenv $1 $version
  else
    echo 'Usage: chr (shell|local|global)'
  fi
}

# create dir $1 and cd into it, creating subdirectories as necessary
function mcd() {
  mkdir -p "$1" && cd "$1";
}

# pretty-print the command search path
function pp() {
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
function update_dotfiles() {
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

# generate a playground project to test out a feature, library, etc.
# usage: play FRAMEWORK PROJECT-NAME
function play() {
  if [[ $1 =~ ^rails$ ]]; then
    rplay -n $2 --skip-bundle
  elif [[ $1 =~ ^(express|play|flask|jade|om|spark)$ ]]; then
    echo "$1 playgrounds haven't been set up yet"
  else
    echo 'Usage example: play rails test_project'
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

# run specs
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

alias s='bundle_or_bin rspec --format=progress .'
alias ss='bundle_or_bin rspec --format=documentation .'
alias rk='bundle_or_bin rake'

alias rs='bundle_or_bin rails server'
alias rc='bundle_or_bin rails console'
alias rcs='bundle_or_bin rails console --sandbox'

alias rss='rk db:reset db:seed && rs'
