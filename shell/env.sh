# shell/env.sh
#-------------------------------------------------------------
# GO
#-------------------------------------------------------------
export GOPATH=$HOME/.go
export GOROOT=/usr/local/opt/go/libexec

#-------------------------------------------------------------
# GNU TOOLS (prepend to PATH)
#   omit coreutils for now, because github/enterprise
#-------------------------------------------------------------
if [[ -z $GNUTOOLS_PATHS || -z $GNUTOOLS_MANS ]]; then
  gnu_tools=(gnu-bin gnu-indent gnu-sed gnu-tar gnu-which gnutls)

  for gnu_tool in "${gnu_tools[@]}"; do
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
# PATH
#-------------------------------------------------------------
PATH="./bin:$HOME/.bin"            # user binaries
PATH+=$GNUTOOLS_PATHS              # GNU command-line tools
PATH+=":$GOPATH/bin:$GOROOT/bin"   # Go binaries
PATH+=":$HOME/.gem/ruby/2.0.0/bin" # user gems for system ruby
PATH+=":/usr/local/heroku/bin"     # heroku-toolbelt binaries
PATH+=":/usr/local/bin"            # homebrewed binaries
PATH+=":/usr/bin:/bin"             # system binaries
PATH+=":/usr/sbin:/sbin"           # system binaries requiring root
PATH+=":/opt/X11/bin"              # added by OSX
PATH+=":/Library/TeX/texbin"       # for TeX
export PATH

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
# RBENV
#-------------------------------------------------------------
if which rbenv > /dev/null; then
  eval "$(rbenv init - --no-rehash)"
fi

#-------------------------------------------------------------
# EXENV
#-------------------------------------------------------------
if which exenv > /dev/null; then
  eval "$(exenv init -)";
fi

#-------------------------------------------------------------
# GPG
#-------------------------------------------------------------
if [[ -f "${HOME}/.gpg-agent-info" ]]; then
  source "$HOME/.gpg-agent-info"
  export GPG_AGENT_INFO
  export SSH_AUTH_SOCK
  export SSH_AGENT_PID
else
  gpg-agent --daemon \
    --enable-ssh-support \
    --write-env-file \
    "${HOME}/.gpg-agent-info"
fi

GPG_TTY=$(tty)
export GPG_TTY
