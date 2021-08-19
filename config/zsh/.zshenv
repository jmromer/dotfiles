printf "${ZDOTDIR/$HOME/~}/.zshenv ... "

source "${HOME}/.dotfiles/env/xdg.sh"
source "${DOTFILES_DIR}/env/asdf.sh"
source "${ASDF_DIR}/asdf.sh"

[ -d /opt/homebrew ] && eval "$(/opt/homebrew/bin/brew shellenv)"
export BREW_PREFIX="$(brew --prefix)"

PATH="${BREW_PREFIX}/opt/coreutils/libexec/gnubin:${PATH}"
PATH="${DOTFILES_DIR}/bin:${PATH}"
PATH="${XDG_CONFIG_HOME}/emacs/bin:${PATH}"
export PATH
