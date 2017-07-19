#!/usr/bin/env bash

#-------------------------------------------------------------
# Emacs
#-------------------------------------------------------------
if [ -n "$INSIDE_EMACS" ]; then
  export EDITOR=emacsclient
fi

#-------------------------------------------------------------
# GPG
#-------------------------------------------------------------
if [ ! -f "${HOME}/.gpg-agent-info" ]; then
  gpg-agent \
    --daemon \
    --write-env-file "${HOME}/.gpg-agent-info" >/dev/null
fi

# shellcheck source=/dev/null
source "${HOME}/.gpg-agent-info"
export GPG_AGENT_INFO

GPG_TTY=$(tty)
export GPG_TTY

#-------------------------------------------------------------
# MacTeX
#-------------------------------------------------------------
export TEXMFVAR="$HOME/Library/texlive/2016/texmf-var"
export TEXMFCONFIG="$HOME/Library/texlive/2016/texmf-config"
export TEXMFSYSVAR="$TEXMFVAR"
export TEXMFSYSCONFIG="$TEXMFCONFIG"
export TEXMFLOCAL="$HOME/Library/texlive/2016/texmf-local"
export TEXDIR="$HOME/Library/texlive/2016"
export TEXMFHOME="$HOME/Library/texmf"

#-------------------------------------------------------------
# Python / Pyenv
#-------------------------------------------------------------
export PYTHON_CONFIGURE_OPTS="--enable-framework"

#-------------------------------------------------------------
# N / NODE
#-------------------------------------------------------------
export N_PREFIX="$HOME/.node"

#-------------------------------------------------------------
# GO
#-------------------------------------------------------------
export GOPATH="$HOME/.go"
export GOROOT="/usr/local/go"

#-------------------------------------------------------------
# ELM
#-------------------------------------------------------------
export ELM_HOME="$HOME/.elm"

#-------------------------------------------------------------
# Android
#-------------------------------------------------------------
export ANDROID_SDK_ROOT=~/Library/Android/sdk
export ANDROID_SDK="$ANDROID_SDK_ROOT"
export ANDROID_HOME="$ANDROID_SDK_ROOT"

#-------------------------------------------------------------
# JAVA
#-------------------------------------------------------------
java_path=/usr/libexec/java_home

if [[ -z "$JAVA_HOME" && -f "$java_path" ]]; then
  JAVA_HOME=$($java_path)
  export JAVA_HOME
  launchctl setenv JAVA_HOME "$JAVA_HOME"
fi

#-------------------------------------------------------------
# SDK MANAGER
#-------------------------------------------------------------
#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/jkrmr/.sdkman"
[[ -s "/Users/jkrmr/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/jkrmr/.sdkman/bin/sdkman-init.sh"
