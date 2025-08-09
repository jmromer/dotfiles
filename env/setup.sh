#!/usr/bin/env bash

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

num_cores() {
  if [[ "${MACHINE}" == "linux" ]]; then
    nproc
  elif command -v /usr/sbin/sysctl >/dev/null; then
    /usr/sbin/sysctl -n hw.ncpu
  else
    echo 8
  fi
}

MACHINE_CORES=$(echo "$(num_cores) - 1" | bc)

export MACHINE
export MACHINE_CORES
export HOMEBREW_PREFIX

# XDG setup
# -----------------------------
source "${DOTFILES_DIR}/env/system_zsh.sh"
source "${DOTFILES_DIR}/env/xdg.sh"

# Environment setup
# -----------------------------

if [[ "${MACHINE}" == "apple" ]]; then
  source "${DOTFILES_DIR}/env/build.sh"
fi

source "${DOTFILES_DIR}/env/bundler.sh"
source "${DOTFILES_DIR}/env/docker.sh"
source "${DOTFILES_DIR}/env/emacs.sh"
source "${DOTFILES_DIR}/env/erlang.sh"
source "${DOTFILES_DIR}/env/fzf.sh"
source "${DOTFILES_DIR}/env/gpg.sh"
source "${DOTFILES_DIR}/env/gtags.sh"
source "${DOTFILES_DIR}/env/kubernetes.sh"
source "${DOTFILES_DIR}/env/mac.sh"
source "${DOTFILES_DIR}/env/python.sh"
source "${DOTFILES_DIR}/env/rails.sh"
source "${DOTFILES_DIR}/env/versions.sh"

# PATH setup
# -----------------------------
source "${DOTFILES_DIR}/env/path.sh"
