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
  exenv                   # elixir version management
  elixir-build            # elixir version installer
  gpg2                    # for PGP commit signing
  gpg-agent
  pinentry-mac
  heroku-toolbelt
  hub                     # For github-flavored git
  imagemagick
  node
  openssl
  pgcli                   # Postgres CLI
  postgres
  python                  # Python 2.7 + Pip
  qt
  shellcheck
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
for formula in rcm parity; do
  brew install thoughtbot/formulae/"$formula"
done


#-------------------------------------------------------------
# Neovim
#-------------------------------------------------------------
# Python, Pip, Buster
pip install --upgrade pip setuptools
pip install --upgrade buster
brew install neovim/neovim/neovim
pip install vim-vint neovim


#-------------------------------------------------------------
# Emacs Installation
#-------------------------------------------------------------
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-modern-icon

#-------------------------------------------------------------
# MacVim Installation
# - Take advantage of MacVim's faster rendering engine
#-------------------------------------------------------------
# Note: Ensure MacVim and YouCompleteMe are compiled against non-system Python

# Use the MacVim binary as CLI vim
# Enable client-server (allows opening gui vim from cli vim with :guie
# with cscope, for tags database
# with lua (improves performance for plugins leveraging it)
# use HEAD

brew install macvim \
  --with-override-system-vim  \
  --with-client-server  \
  --with-cscope  \
  --with-lua  \
  --HEAD
