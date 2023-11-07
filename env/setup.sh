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
  # n-1: http://archlever.blogspot.com/2013/09/lies-damned-lies-and-truths-backed-by.html
  MACHINE_CORES=$(echo "$(/usr/sbin/sysctl -n hw.ncpu) - 1" | bc)

  IN_ROSETTA="$(/usr/sbin/sysctl -in sysctl.proc_translated)"
  if [ "${IN_ROSETTA:-0}" -eq "1" ]; then
    MACHINE="apple"
    HOMEBREW_PREFIX="/opt/homebrew"
  fi
else
  MACHINE_CORES=8
  echo "Warning: MACHINE_CORES set to default value of ${MACHINE_CORES}."
fi

export MACHINE
export MACHINE_CORES
export HOMEBREW_PREFIX

[ -x /opt/homebrew/bin/brew ] && eval "$(/opt/homebrew/bin/brew shellenv)"

# XDG setup
# -----------------------------
export DOTFILES_DIR="${HOME}/.dotfiles"
source "${DOTFILES_DIR}/env/xdg.sh"

# Environment setup
# -----------------------------

source "${DOTFILES_DIR}/env/asdf.sh"
source "${DOTFILES_DIR}/env/build.sh"
source "${DOTFILES_DIR}/env/bundler.sh"
source "${DOTFILES_DIR}/env/docker.sh"
source "${DOTFILES_DIR}/env/emacs.sh"
source "${DOTFILES_DIR}/env/erlang.sh"
source "${DOTFILES_DIR}/env/fzf.sh"
source "${DOTFILES_DIR}/env/golang.sh"
source "${DOTFILES_DIR}/env/gpg.sh"
source "${DOTFILES_DIR}/env/gtags.sh"
source "${DOTFILES_DIR}/env/kubernetes.sh"
source "${DOTFILES_DIR}/env/mac.sh"
source "${DOTFILES_DIR}/env/python.sh"
source "${DOTFILES_DIR}/env/rails.sh"

# PATH setup
# -----------------------------
source "${DOTFILES_DIR}/env/path.sh"
