# shell/env.sh
#-------------------------------------------------------------
# PATH
#-------------------------------------------------------------
PATH="/usr/local/heroku/bin:"           # heroku-toolbelt binaries
PATH+="/usr/local/bin:/usr/local/sbin:" # homebrewed binaries
PATH+="/usr/bin:/bin:/usr/sbin:/sbin:"  # system binaries
PATH+="/opt/X11/bin:"                   # added by OSX
PATH+="/usr/texbin"                     # for TeX
PATH+="$HOME/.bin:"                     # user binaries
PATH+="."                               # current directory (must come last)

#-------------------------------------------------------------
# MAN PAGES
#-------------------------------------------------------------
MANPATH="$(manpath)"
MANPATH="/usr/local/man:$MANPATH"

#-------------------------------------------------------------
# GNU TOOLS (prepend to PATH and MANPATH)
#-------------------------------------------------------------
gnu_tools=(
  coreutils
  gnu-bin
  gnu-indent
  gnu-sed
  gnu-tar
  gnu-which
  gnutls
)

for gnu_tool in "${gnu_tools[@]}"; do
  PATH="/usr/local/opt/$gnu_tool/libexec/gnubin:$PATH"
  MANPATH="/usr/local/opt/$gnu_tool/libexec/gnuman:$MANPATH"
done

#-------------------------------------------------------------
# CD PATH
#-------------------------------------------------------------
CDPATH=".:$HOME/Desktop:$HOME/Developer:$HOME"

#-------------------------------------------------------------
# EXPORT PATH vars
#-------------------------------------------------------------
export PATH
export MANPATH
export CDPATH

#-------------------------------------------------------------
# SYNTAX HIGHLIGHTING
#-------------------------------------------------------------
export CLICOLOR=1  # BSD
export LSCOLORS=exfxcxdxbxegedabagacad
export LS_COLORS="di=00;34:ex=00;31:ln=00;32:or=00;35:mi=90;30"  # GNU
export TERM=xterm-256color  # Set colors to match iTerm2 Terminal Colors

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
# MISC ENV VARIABLES
#-------------------------------------------------------------
export EDITOR="vim"
export PAGER="less"
export JAVA_HOME="$(/usr/libexec/java_home)"
export HISTCONTROL=ignoreboth   # Ignore spaces and duplicates
export HISTIGNORE="??:&:pwd:cd*:h:..*:l:ll:ll?:q:c:l:g"
#-------------------------------------------------------------
# API KEYS
#-------------------------------------------------------------
export HOMEBREW_GITHUB_API_TOKEN
export AWS_ACCESS_KEY_ID
export AWS_SECRET_ACCESS_KEY

#-------------------------------------------------------------
# RUBY MANAGER
#-------------------------------------------------------------
if which rbenv > /dev/null; then
  eval "$(rbenv init -)"   # execute rbenv loading script
elif [[ -s "$HOME/.rvm/scripts/rvm" ]]; then
  . "$HOME/.rvm/scripts/rvm"         # load RVM as a function
  export PATH="$HOME/.rvm/bin:$PATH" # prepend RVM to PATH for scripting
fi

