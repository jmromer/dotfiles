# shell/env.sh
#-------------------------------------------------------------
# GO
#-------------------------------------------------------------
export GOPATH=$HOME/.go
export GOROOT=/usr/local/opt/go/libexec

#-------------------------------------------------------------
# GNU TOOLS (prepend to PATH)
#-------------------------------------------------------------
if [[ -z $GNUTOOLS_PATHS || -z $GNUTOOLS_MANS ]]; then
  gnu_tools=( coreutils gnu-bin gnu-indent gnu-sed gnu-tar gnu-which gnutls )

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
MANPATH+="$(manpath)"
export MANPATH

#-------------------------------------------------------------
# PATH
#-------------------------------------------------------------
PATH="$HOME/.bin:$HOME/bin"      # user binaries
PATH+=$GNUTOOLS_PATHS            # GNU command-line tools
PATH+=":$GOPATH/bin:$GOROOT/bin" # Go binaries
PATH+=":/usr/local/heroku/bin"   # heroku-toolbelt binaries
PATH+=":/usr/local/bin"          # homebrewed binaries
PATH+=":/usr/bin:/bin"           # system binaries
PATH+=":/usr/sbin:/sbin"         # system binaries requiring root
PATH+=":/opt/X11/bin"            # added by OSX
PATH+=":/usr/texbin"             # for TeX
PATH+=":."                       # current directory (must come last)
export PATH

#-------------------------------------------------------------
# CD PATH
#-------------------------------------------------------------
export CDPATH=".:$HOME/Desktop:$HOME/Developer:$HOME"

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
export EDITOR="vim"
export PAGER="less"

# brew install [or sudo apt-get install] source-highlight
export LESSOPEN="| /usr/local/bin/src-hilite-lesspipe.sh %s"
export LESS=' -R '

#-------------------------------------------------------------
# MISC ENV VARIABLES
#-------------------------------------------------------------
export JAVA_HOME="$(/usr/libexec/java_home)"
export HISTCONTROL=ignoreboth   # Ignore spaces and duplicates
export HISTIGNORE="??:&:pwd:cd*:h:..*:l:ll:ll?:q:c:l:g"

#-------------------------------------------------------------
# API KEYS
#-------------------------------------------------------------
export HOMEBREW_GITHUB_API_TOKEN

#-------------------------------------------------------------
# Use Ag for FZF instead of find
#-------------------------------------------------------------
export FZF_DEFAULT_COMMAND='ag -l -g ""'

#-------------------------------------------------------------
# RUBY MANAGER
#-------------------------------------------------------------
if which rbenv > /dev/null; then
  eval "$(rbenv init -)"   # execute rbenv loading script
elif [[ -s "$HOME/.rvm/scripts/rvm" ]]; then
  . "$HOME/.rvm/scripts/rvm"         # load RVM as a function
  export PATH="$HOME/.rvm/bin:$PATH" # prepend RVM to PATH for scripting
fi

#-------------------------------------------------------------
# Pyenv
#-------------------------------------------------------------
if which pyenv > /dev/null; then
  eval "$(pyenv init -)";
fi
