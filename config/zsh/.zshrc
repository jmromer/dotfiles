echo "[Loading $ZDOTDIR/.zshrc ...]"

source "${HOME}/.dotfiles/env/xdg.sh"
source "${HOME}/.dotfiles/env/asdf.sh"
source "$ASDF_DIR/asdf.sh"

eval "$(/opt/homebrew/bin/brew shellenv)"
export BREW_PREFIX="$(brew --prefix)"

PATH="${BREW_PREFIX}/opt/coreutils/libexec/gnubin:${PATH}"
PATH="${HOME}/.dotfiles/bin:${PATH}"
PATH="${HOME}/.dotfiles/config/emacs/bin:${PATH}"
export PATH


fpath=(
  ${BREW_PREFIX}/share/zsh/site-functions
  ${ASDF_DIR}/completions 
  $fpath
)
autoload -Uz compinit
compinit

export PS1="%1~ %# "
