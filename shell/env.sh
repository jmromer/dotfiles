#!/usr/bin/env bash

uname_out="$(uname -s)"
case "${uname_out}" in
  Linux*)  machine=linux;;
  Darwin*) machine=mac;;
  CYGWIN*) machine=windows;;
  *)       machine="UNKNOWN:${uname_out}"
esac
export MACHINE="$machine"

#-------------------------------------------------------------
# Set HOMEBREW_PREFIX
#-------------------------------------------------------------
if [[ "$MACHINE" == "linux" ]]; then
  HOMEBREW_PREFIX="/home/linuxbrew/.linuxbrew"
else
  HOMEBREW_PREFIX="/usr/local"
fi
export HOMEBREW_PREFIX

#-------------------------------------------------------------
# Emacs
#-------------------------------------------------------------
if [ -n "$INSIDE_EMACS" ]; then
  export EDITOR=emacs
fi

#-------------------------------------------------------------
# GPG
#-------------------------------------------------------------
GPG_TTY=$(tty)
export GPG_TTY

#-------------------------------------------------------------
# Python
#-------------------------------------------------------------
export PYTHONDONTWRITEBYTECODE=1

# http://vi.stackexchange.com/a/7654
if [[ -n $VIRTUAL_ENV && -e "${VIRTUAL_ENV}/bin/activate" ]]; then
  source "${VIRTUAL_ENV}/bin/activate"
fi

#-------------------------------------------------------------
# N/Node, Anaconda, Golang, Elm
#-------------------------------------------------------------
export ANACONDA_PREFIX="$HOME/.anaconda"
export ANACONDA_HOME="$HOME/.anaconda"
export ELM_HOME="$HOME/.elm"
export ERL_AFLAGS="-kernel shell_history enabled"
unset GOPATH
unset GOROOT

#-------------------------------------------------------------
# Use Ripgrep for FZF instead of find
#-------------------------------------------------------------
export FZF_DIR="$HOMEBREW_PREFIX/opt/fzf"
export FZF_DEFAULT_COMMAND='rg --files-with-matches "" 2>/dev/null'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_DEFAULT_OPTS="--no-multi --ansi --tiebreak=index --bind='ctrl-f:preview-down' --bind='ctrl-b:preview-up'"

#-------------------------------------------------------------
# Android
#-------------------------------------------------------------
export ANDROID_SDK_ROOT=~/Library/Android/sdk
export ANDROID_SDK="$ANDROID_SDK_ROOT"
export ANDROID_HOME="$ANDROID_SDK_ROOT"

#-------------------------------------------------------------
# JAVA
#-------------------------------------------------------------
# java_path=/usr/libexec/java_home
# JAVA_HOME=$($java_path)

JAVA_HOME="$HOME/.asdf/installs/java/openjdk-11/"
export JAVA_HOME

if [[ "$MACHINE" == "mac" ]]; then
  launchctl setenv JAVA_HOME "$JAVA_HOME"
fi

#-------------------------------------------------------------
# DOCKER
#-------------------------------------------------------------
export DOCKER_HIDE_LEGACY_COMMANDS=true

#-------------------------------------------------------------
# GTAGS
#-------------------------------------------------------------
export GTAGSLABEL=pygments

#-------------------------------------------------------------
# Compilation flags
#-------------------------------------------------------------
export LDFLAGS="-L/usr/local/opt/gettext/lib -L/usr/local/opt/libffi/lib -L/usr/local/opt/readline/lib"
export CPPFLAGS="-I/usr/local/opt/gettext/include -I/usr/local/opt/readline/include"
export PKG_CONFIG_PATH="/usr/local/opt/libffi/lib/pkgconfig:/usr/local/opt/readline/lib/pkgconfig"
