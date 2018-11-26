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
MANPATH+="/usr/local/opt/coreutils/libexec/gnuman:"
MANPATH+="$(manpath)"
export MANPATH

#-------------------------------------------------------------
# PATH
#-------------------------------------------------------------
PATH="$HOME/.bin"                  # user binaries
PATH+=$GNUTOOLS_PATHS              # GNU command-line tools
PATH+=":$ANACONDA_PREFIX/bin"      # Anaconda binaries
PATH+=":$HOME/.local/bin"          # haskell binaries
PATH+=":$N_PREFIX/bin"             # n version binaries
PATH+=":$HOME/.fzf/bin"            # FZF fuzzy-finder
PATH+=":$HOME/.rbenv/bin"          # Rbenv binary path
PATH+=":$HOME/.gem/ruby/2.5.0/bin" # user-install gems for system ruby
PATH+=":$HOME/.exenv/bin"          # Exenv binary path
PATH+=":$HOME/.cargo/bin"          # Rust binary path
PATH+=":$GOPATH/bin:$GOROOT/bin"   # Go binaries
PATH+=":./node_modules/.bin"       # Project-local node binaries
PATH+=":/usr/local/heroku/bin"     # heroku-toolbelt binaries
PATH+=":/usr/local/bin"            # homebrewed binaries
PATH+=":/usr/local/sbin"           # homebrewed binaries
PATH+=":/usr/bin:/bin"             # system binaries
PATH+=":/usr/sbin:/sbin"           # system binaries requiring root
PATH+=":/opt/X11/bin"              # added by OSX
PATH+=":$HOME/Library/Android/sdk/tools/bin" # Android SDK CLT binaries
PATH+=":/Library/TeX/texbin"
export PATH

#-------------------------------------------------------------
# RBENV
#-------------------------------------------------------------
if command -v rbenv > /dev/null; then
  eval "$(rbenv init - --no-rehash)"
fi

# Ensure RBENV_VERSION is unset on shell start-up
unset RBENV_VERSION

#-------------------------------------------------------------
# EXENV
#-------------------------------------------------------------
if command -v exenv > /dev/null; then
  eval "$(exenv init -)";
fi
