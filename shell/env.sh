#!/usr/bin/env bash

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
  *)
      MACHINE="UNKNOWN"
      HOMEBREW_PREFIX="$(brew --prefix)"
      ;;
esac

export MACHINE
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
# Elm
#-------------------------------------------------------------
export ELM_HOME="$HOME/.elm"

#-------------------------------------------------------------
# Erlang
#-------------------------------------------------------------
export ERL_AFLAGS="-kernel shell_history enabled"
export KERL_BUILD_DOCS=no
export KERL_CONFIGURE_OPTIONS="--disable-debug --without-javac --disable-hipe"

#-------------------------------------------------------------
# Anaconda
# (symlink to asdf install directory)
#-------------------------------------------------------------
export ANACONDA_PREFIX="$HOME/.anaconda"
export ANACONDA_HOME="$HOME/.anaconda"

#-------------------------------------------------------------
# Go
#-------------------------------------------------------------
export GO_VERSION_DEFAULT=1.14.1
export GOROOT=~/.asdf/installs/golang/${GO_VERSION:-$GO_VERSION_DEFAULT}/go/
export GOPATH=~/.go

#-------------------------------------------------------------
# Use Ripgrep for FZF instead of find
#-------------------------------------------------------------
export FZF_DIR="$HOMEBREW_PREFIX/opt/fzf"
export FZF_DEFAULT_COMMAND='rg --files-with-matches "" 2>/dev/null'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_DEFAULT_OPTS="--no-multi --tiebreak=index --bind='ctrl-f:preview-down' --bind='ctrl-b:preview-up'"

#-------------------------------------------------------------
# Android
#-------------------------------------------------------------
export ANDROID_SDK_ROOT=~/Library/Android/sdk
export ANDROID_SDK="$ANDROID_SDK_ROOT"
export ANDROID_HOME="$ANDROID_SDK_ROOT"

#-------------------------------------------------------------
# JAVA
#-------------------------------------------------------------
JAVA_VERSION=openjdk-15.0.1
JAVA_HOME="$HOME/.asdf/installs/java/${JAVA_VERSION}"
export JAVA_HOME

if [[ "$MACHINE" =~ (apple|intel-mac) ]]; then
  launchctl setenv JAVA_HOME "$JAVA_HOME"
fi

#-------------------------------------------------------------
# DOCKER
#-------------------------------------------------------------
export DOCKER_HIDE_LEGACY_COMMANDS=true

#-------------------------------------------------------------
# GTAGS
#-------------------------------------------------------------
export GTAGSLABEL=ctags
export GTAGSCONF="${HOME}/.globalrc"

#-------------------------------------------------------------
# Compilation flags
#-------------------------------------------------------------
export CC=gcc
export CXX=g++

LDFLAGS="-L$HOMEBREW_PREFIX/opt/gettext/lib"
LDFLAGS+=" -L$HOMEBREW_PREFIX/opt/libffi/lib"
LDFLAGS+=" -L$HOMEBREW_PREFIX/opt/libxml2/lib"
LDFLAGS+=" -L$HOMEBREW_PREFIX/opt/llvm/lib -Wl,-rpath,$HOMEBREW_PREFIX/opt/llvm/lib"
LDFLAGS+=" -L$HOMEBREW_PREFIX/opt/ncurses/lib"
LDFLAGS+=" -L$HOMEBREW_PREFIX/opt/openssl/lib"
LDFLAGS+=" -L$HOMEBREW_PREFIX/opt/icu4c/lib"
LDFLAGS+=" -L$HOMEBREW_PREFIX/opt/readline/lib"
LDFLAGS+=" -L$HOMEBREW_PREFIX/opt/zlib/lib"
export LDFLAGS

CPPFLAGS="-I$HOMEBREW_PREFIX/opt/gettext/include"
CPPFLAGS+=" -I$HOMEBREW_PREFIX/opt/libxml2/include"
CPPFLAGS+=" -I$HOMEBREW_PREFIX/opt/llvm/include"
CPPFLAGS+=" -I$HOMEBREW_PREFIX/opt/ncurses/include"
CPPFLAGS+=" -I$HOMEBREW_PREFIX/opt/openssl/include"
CPPFLAGS+=" -I$HOMEBREW_PREFIX/opt/icu4c/include"
CPPFLAGS+=" -I$HOMEBREW_PREFIX/opt/readline/include"
CPPFLAGS+=" -I$HOMEBREW_PREFIX/opt/zlib/include"
export CPPFLAGS

PKG_CONFIG_PATH="$HOMEBREW_PREFIX/opt/libffi/lib/pkgconfig"
PKG_CONFIG_PATH+=":$HOMEBREW_PREFIX/opt/libxml2/lib/pkgconfig"
PKG_CONFIG_PATH+=":$HOMEBREW_PREFIX/opt/ncurses/lib/pkgconfig"
PKG_CONFIG_PATH+=":$HOMEBREW_PREFIX/opt/openssl/lib/pkgconfig"
PKG_CONFIG_PATH+=":$HOMEBREW_PREFIX/opt/icu4c/lib/pkgconfig"
PKG_CONFIG_PATH+=":$HOMEBREW_PREFIX/opt/readline/lib/pkgconfig"
PKG_CONFIG_PATH+=":$HOMEBREW_PREFIX/opt/zlib/lib/pkgconfig"
PKG_CONFIG_PATH+=":$HOMEBREW_PREFIX/opt/imagemagick/lib/pkgconfig"
export PKG_CONFIG_PATH

RUBY_CONFIGURE_OPTS="--with-readline-dir=$HOMEBREW_PREFIX/opt/readline"
RUBY_CONFIGURE_OPTS+=" --with-openssl-dir=$HOMEBREW_PREFIX/opt/openssl"
RUBY_CONFIGURE_OPTS+=" --enable-shared"
RUBY_CONFIGURE_OPTS+=" --disable-libedit"
export RUBY_CONFIGURE_OPTS

export CFLAGS="-O3 -g -I$HOMEBREW_PREFIX/opt/openssl/include"
export ARCHFLAGS="-arch x86_64"
export RUBY_CFLAGS="-march=native -Os"
export RUBY_GC_MALLOC_LIMIT=60000000
export RUBY_GC_HEAP_FREE_SLOTS=200000

#-------------------------------------------------------------
# Spring: Enable for parallel_tests
#-------------------------------------------------------------
export DISABLE_SPRING=0

#-------------------------------------------------------------
# ASDF: extra config options for installs
#-------------------------------------------------------------
export POSTGRES_EXTRA_CONFIGURE_OPTIONS=--with-uuid=e2fs
