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
PATH+=":$HOME/.local/bin"          # haskell binaries
PATH+=":$HOME/.exenv/bin"          # Exenv binary path
PATH+=":$HOME/.pyenv/bin"          # Pyenv binary path
PATH+=":$HOME/.cargo/bin"          # Rust binary path
PATH+=":$GOPATH/bin:$GOROOT/bin"   # Go binaries
PATH+=":$HOME/.gem/ruby/2.0.0/bin" # user gems for system ruby
PATH+=":$N_PREFIX/bin"             # n version binaries
PATH+=":$HOME/.fzf/bin"            # FZF fuzzy-finder
PATH+=":$HOME/.gem/ruby/2.3.0/bin" # user-install system Ruby gems
PATH+=":/usr/local/heroku/bin"     # heroku-toolbelt binaries
PATH+=":/usr/local/bin"            # homebrewed binaries
PATH+=":/usr/local/sbin"           # homebrewed binaries
PATH+=":/usr/bin:/bin"             # system binaries
PATH+=":/usr/sbin:/sbin"           # system binaries requiring root
PATH+=":/opt/X11/bin"              # added by OSX
PATH+=":/usr/local/opt/qt@5.5/bin" # QT 5.5, for capybara-webkit
PATH+=":$HOME/.anaconda3/bin"      # Anaconda binaries
PATH+=":/Library/TeX/texbin"
PATH+=":/Library/Developer/Toolchains/swift-latest.xctoolchain/usr/bin"
PATH+=":$HOME/Library/texlive/2016/bin/x86_64-darwin"
export PATH

#-------------------------------------------------------------
# RBENV
#-------------------------------------------------------------
if command -v rbenv > /dev/null; then
  eval "$(rbenv init - --no-rehash)"
fi

#-------------------------------------------------------------
# EXENV
#-------------------------------------------------------------
if command -v exenv > /dev/null; then
  eval "$(exenv init -)";
fi

#-------------------------------------------------------------
# PYENV
#-------------------------------------------------------------
if command -v pyenv > /dev/null; then
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"
fi

#-------------------------------------------------------------
# OCaml env config
#-------------------------------------------------------------
. $HOME/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

eval $(opam config env)
