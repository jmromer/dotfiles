#-------------------------------------------------------------
# Universal Ctags
#-------------------------------------------------------------
brew install --HEAD universal-ctags/universal-ctags/universal-ctags

#-------------------------------------------------------------
# Exuberant Ctags + Global
#-------------------------------------------------------------
# brew install ctags --HEAD
# brew install global --with-ctags --with-pygments

#-------------------------------------------------------------
# Git
#-------------------------------------------------------------
brew install openssl
brew install --with-openssl curl
brew install --with-brewed-curl --with-brewed-openssl git

#-------------------------------------------------------------
# Homebrewed Packages
#-------------------------------------------------------------
homebrew=(
  awscli                  # AWS command line interface
  bash                    # Updated version of Bash
  bash-completion         # Command completions for Bash
  cmake                   # For YCM installation
  colordiff
  dfu-util                # KLL configuration loader
  gpg2                    # for PGP commit signing
  gpg-agent
  pinentry-mac
  heroku-toolbelt
  hub                     # For github-flavored git
  imagemagick
  ispell
  pgcli                   # Postgres CLI
  postgres
  pyenv
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
  zsh                     # Updated version of Zshell
  zsh-completions         # Command completions for Zshell
  zsh-syntax-highlighting # Syntax highlighting as you type
)

for package in ${homebrew[*]}; do
  echo "Installing or upgrading $package..." && echo
  brew install $package
done

#-------------------------------------------------------------
# Thoughbot
#-------------------------------------------------------------
brew tap thoughtbot/formulae

brew install rcm parity gitsh

#-------------------------------------------------------------
# Neovim
#-------------------------------------------------------------
# Python, Pip
brew install neovim/neovim/neovim
mkdir "$HOME/.config"
ln -s "$HOME/.vim" "$HOME/.config/nvim"
gem install neovim


#-------------------------------------------------------------
# Emacs Installation
#-------------------------------------------------------------
brew tap d12frosted/emacs-plus
brew install emacs-plus
brew linkapps emacs-plus
brew services start d12frosted/emacs-plus/emacs-plus

tic -o ~/.terminfo /usr/local/share/emacs/24.5/etc/e/eterm-color.ti

#-------------------------------------------------------------
# MacVim Installation
# - Take advantage of MacVim's faster rendering engine
#-------------------------------------------------------------
# Note: Ensure everything is compiled against non-system Python

# Use the MacVim binary as CLI vim
# with lua (improves performance for plugins leveraging it)
# use HEAD

brew install macvim \
  --with-override-system-vim  \
  --with-python3 \
  --with-lua  \
  --HEAD

brew linkapps macvim
