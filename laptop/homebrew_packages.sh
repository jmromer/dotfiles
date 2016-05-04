#-------------------------------------------------------------
# Universal Ctags
#-------------------------------------------------------------
brew install --HEAD universal-ctags/universal-ctags/universal-ctags

#-------------------------------------------------------------
# Git
#-------------------------------------------------------------
brew install git --with-brewed-curl --with-brewed-openssl

#-------------------------------------------------------------
# Homebrewed Packages
#-------------------------------------------------------------
homebrew=(
  awscli                  # AWS command line interface
  bash                    # Updated version of Bash
  bash-completion         # Command completions for Bash
  cmake                   # For YCM installation
  dfu-util                # KLL configuration loader
  emacs                   # emacs duh
  gpg                     # for PGP commit signing
  gpg-agent
  pinentry-mac
  gtypist                 # Touch-type training
  heroku-toolbelt
  hub                     # For github-flavored git
  imagemagick
  n                       # Node environment manager
  node
  openssl
  pgcli                   # Postgres CLI
  postgres
  python                  # Python 2.7 + Pip
  qt
  rbenv
  rbenv-default-gems      # default gems to be installed
  reattach-to-user-namespace
  redis
  ruby-build
  source-highlight        # syntax highlighting for less
  the_silver_searcher
  tmux
  tree                    # for viewing directory contents in tree format
  vim
  zsh                     # Updated version of Zshell
  zsh-completions         # Command completions for Zshell
  zsh-syntax-highlighting # Syntax highlighting as you type
)

for package in ${homebrew[*]}; do
  echo "Installing or upgrading $package..." && echo
  brew install --force $package
done

#-------------------------------------------------------------
# Thoughbot
#-------------------------------------------------------------
for formula in rcm liftoff parity; do
  brew install --force thoughtbot/formulae/"$formula"
done

#-------------------------------------------------------------
# MacVim Installation
# - Take advantage of MacVim's faster rendering engine
#-------------------------------------------------------------
# Note: Ensure MacVim and YouCompleteMe are compiled against non-system Python

# Use the MacVim binary as CLI vim
options=' --with-override-system-vim '

# Enable client-server (allows opening gui vim from cli vim with :gui)
options+=' --with-client-server '

# with cscope, for tags database
options+=' --with-cscope '

# with lua (improves performance for plugins leveraging it)
options+=' --with-lua '

# use HEAD
options+=' --HEAD '

brew install macvim $options
