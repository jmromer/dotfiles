#!/usr/bin/env sh

# Homebrew setup
# -----------------------------
case "$(uname -ps)" in
  Linux*)
    MACHINE="linux"
    HOMEBREW_PREFIX="/home/linuxbrew/.linuxbrew"
  ;;
  Darwin\ arm*)
    MACHINE="apple"
    HOMEBREW_PREFIX="/opt/homebrew"
  ;;
  Darwin*)
    MACHINE="intel-mac"
    HOMEBREW_PREFIX="/usr/local"
  ;;
esac

if command -v /usr/sbin/sysctl >/dev/null; then
  MACHINE_CORES=$(echo "$(/usr/sbin/sysctl -n hw.ncpu) / 2" | bc)
else
  MACHINE_CORES=8
  echo "Warning: MACHINE_CORES set to default value of ${MACHINE_CORES}."
fi

export MACHINE
export MACHINE_CORES
export HOMEBREW_PREFIX

eval "$($HOMEBREW_PREFIX/bin/brew shellenv)"

# XDG setup
# -----------------------------
export DOTFILES_DIR="${HOME}/.dotfiles"
source "${DOTFILES_DIR}/env/xdg.sh"

# Environment setup
# -----------------------------
source "${DOTFILES_DIR}/env/asdf.sh"
source "${DOTFILES_DIR}/env/build.sh"
source "${DOTFILES_DIR}/env/fzf.sh"
source "${DOTFILES_DIR}/env/bundler.sh"
source "${DOTFILES_DIR}/env/gpg.sh"

# PATH setup
# -----------------------------
source "${DOTFILES_DIR}/env/path.sh"

