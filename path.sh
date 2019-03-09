#-------------------------------------------------------------
# GNU TOOLS (prepend to PATH)
#-------------------------------------------------------------
if [ -z "$GNUTOOLS_PATHS" ] || [ -z "$GNUTOOLS_MANS" ]; then
  gnu_tools='coreutils gnu-bin gnu-indent gnu-sed gnu-tar gnu-which gnutls'

  for gnu_tool in $(echo $gnu_tools); do
    GNUTOOLS_PATHS+=":$HOMEBREW_PREFIX/opt/$gnu_tool/libexec/gnubin"
    GNUTOOLS_MANS+="$HOMEBREW_PREFIX/opt/$gnu_tool/libexec/gnuman:"
  done
fi

#-------------------------------------------------------------
# MANPATH
#-------------------------------------------------------------
MANPATH="$GNUTOOLS_MANS"
MANPATH+="$HOMEBREW_PREFIX/opt/erlang/lib/erlang/man:"
MANPATH+="$HOMEBREW_PREFIX/opt/coreutils/libexec/gnuman:"
MANPATH+="$(manpath 2>/dev/null)"
export MANPATH

#-------------------------------------------------------------
# PATH
#-------------------------------------------------------------
PATH="$HOME/.bin"                    # user binaries
PATH+=":$HOME/.local/bin"            # user binaries
PATH+=$GNUTOOLS_PATHS                # GNU command-line tools
PATH+=":$ANACONDA_PREFIX/bin"        # Anaconda binaries
PATH+=":$HOME/.asdf/shims"           # asdf shims
PATH+=":./node_modules/.bin"         # Project-local node binaries
PATH+=":$FZF_DIR/bin"                # FZF fuzzy-finder
PATH+=":$HOME/.gem/ruby/2.5.0/bin"   # user-install gems for system ruby
PATH+=":$HOME/.cargo/bin"            # Rust binary path
PATH+=":$GOPATH/bin:$GOROOT/bin"     # Go binaries
PATH+=":$HOMEBREW_PREFIX/heroku/bin" # heroku-toolbelt binaries
PATH+=":$HOMEBREW_PREFIX/bin"        # homebrewed binaries
PATH+=":$HOMEBREW_PREFIX/sbin"       # homebrewed binaries
PATH+=":/usr/bin:/bin"               # system binaries
PATH+=":/usr/sbin:/sbin"             # system binaries requiring root
PATH+=":/opt/X11/bin"                # added by OSX
PATH+=":$HOME/Library/Android/sdk/tools/bin" # Android SDK CLT binaries
PATH+=":$HOMEBREW_PREFIX/share/git-core/contrib/diff-highlight"
PATH+=":/Library/TeX/texbin"
export PATH
