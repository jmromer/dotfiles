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
  openssl
  pgcli                   # Postgres CLI
  postgres
  python3                 # Python 3 + Pip
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
  brew install --force $package
done

#-------------------------------------------------------------
# Node
#-------------------------------------------------------------
curl -L https://git.io/n-install | bash

n stable

npm install -g \
    babel-eslint \
    coffeelint \
    eslint \
    eslint-plugin-react \
    js-beautify \
    npm \
    react \
    react-native \
    react-native-cli \
    tern \

#-------------------------------------------------------------
# Thoughbot
#-------------------------------------------------------------
brew tap thoughtbot/formulae

for formula in rcm parity gitsh; do
  brew install "$formula"
done


#-------------------------------------------------------------
# Neovim
#-------------------------------------------------------------
# Python, Pip
pip3 install --upgrade pip setuptools neovim vim-vint pygments
brew install neovim/neovim/neovim


#-------------------------------------------------------------
# Emacs Installation
#-------------------------------------------------------------
brew tap d12frosted/emacs-plus

brew install emacs-plus \
     --with-cocoa \
     --with-gnutls \
     --with-librsvg \
     --with-imagemagick \
     --with-spacemacs-icon

brew linkapps emacs-plus

git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
git clone --recursive https://github.com/jkrmr/dot_spacemacs ~/.spacemacs.d

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
