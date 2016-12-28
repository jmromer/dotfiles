#!/usr/bin/env bash

# shell/env.sh
#-------------------------------------------------------------
# GNU TOOLS (prepend to PATH)
#-------------------------------------------------------------
if [ -z "$GNUTOOLS_PATHS" ] || [ -z "$GNUTOOLS_MANS" ]; then
  gnu_tools='coreutils gnu-bin gnu-indent gnu-sed gnu-tar gnu-which gnutls'

  for gnu_tool in $(echo $gnu_tools); do
    GNUTOOLS_PATHS+=":/usr/local/opt/$gnu_tool/libexec/gnubin"
    GNUTOOLS_MANS+="/usr/local/opt/$gnu_tool/libexec/gnuman:"
  done
fi

#-------------------------------------------------------------
# MANPATH
#-------------------------------------------------------------
MANPATH="$GNUTOOLS_MANS"
MANPATH+="/usr/local/opt/erlang/lib/erlang/man:"
MANPATH+="/usr/local/opt/coreutils/libexec/gnuman:" # because GNU ls
MANPATH+="$(manpath)"
export MANPATH

#-------------------------------------------------------------
# CD PATH
#-------------------------------------------------------------
# Ensure CDPATH remains unset
unset CDPATH

#-------------------------------------------------------------
# SYNTAX HIGHLIGHTING
#-------------------------------------------------------------
# BSD
export CLICOLOR=1
export LSCOLORS=exfxcxdxbxegedabagacad

# GNU
export LS_COLORS="di=00;34:ex=00;31:ln=00;32:or=00;35:mi=90;30"

#-------------------------------------------------------------
# RUBY OPTIMIZATIONS
#-------------------------------------------------------------
export ARCHFLAGS="-arch x86_64"   # for compiling gems

# Ruby compilation flags
RUBY_CONFIGURE_OPTS="--with-readline-dir=$(brew --prefix readline) "
RUBY_CONFIGURE_OPTS+="--enable-shared"
export RUBY_CONFIGURE_OPTS
export RUBY_CFLAGS="-march=native -Os"
export CC="clang"
export CXX="clang++"

# Run garbage collection less frequently than default
export RUBY_GC_MALLOC_LIMIT=60000000
export RUBY_GC_HEAP_FREE_SLOTS=200000

#-------------------------------------------------------------
# EDITOR / PAGER
#-------------------------------------------------------------
export EDITOR="nvim"
export PAGER="less"

# brew install [or sudo apt-get install] source-highlight
export LESS=' -r '
export LESSOPEN="| /usr/local/bin/src-hilite-lesspipe.sh %s"

# pager search matches coloring
export LESS_TERMCAP_so=$'\E[30;43m' # black on yellow
export LESS_TERMCAP_se=$'\E[00;00m' # reset

#-------------------------------------------------------------
# MISC ENV VARIABLES
#-------------------------------------------------------------
export HISTCONTROL=ignoreboth   # Ignore spaces and duplicates
export HISTIGNORE="??:&:pwd:cd*:h:..*:l:ll:ll?:q:c:l:g"

#-------------------------------------------------------------
# Use Ag for FZF instead of find
#-------------------------------------------------------------
export FZF_DEFAULT_COMMAND='ag -l -g ""'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_DEFAULT_OPTS='--multi --ansi'

#-------------------------------------------------------------
# Java
#-------------------------------------------------------------
java_path=/usr/libexec/java_home

if [[ -z "$JAVA_HOME" && -f "$java_path" ]]; then
  JAVA_HOME=$($java_path)
  export JAVA_HOME
fi

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
# Go, Elm, N / Node, et al
#-------------------------------------------------------------
# shellcheck source=/dev/null
source ~/.zshenv
