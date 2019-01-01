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
brew install --with-curl --with-openssl git

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
  direnv                  # directory-specific env settings
  gpg2                    # for PGP commit signing
  pinentry-mac
  hub                     # For github-flavored git
  imagemagick
  ispell
  osxutils
  pgcli                   # Postgres CLI
  postgres
  qt
  shellcheck
  rbenv
  rbenv-binstubs
  rbenv-ctags
  rbenv-default-gems      # default gems to be installed
  reattach-to-user-namespace
  redis
  ripgrep
  ruby-build
  source-highlight        # syntax highlighting for less
  the_silver_searcher
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
# Octave
#-------------------------------------------------------------
brew install qt
brew install octave --with-qt
brew install gnuplot --with-qt

#-------------------------------------------------------------
# Neovim
#-------------------------------------------------------------
# Python, Pip
brew install neovim/neovim/neovim
mkdir "$HOME/.config"
ln -s "$HOME/.vim" "$HOME/.config/nvim"
gem install neovim


#-------------------------------------------------------------
# tmux
#-------------------------------------------------------------
brew install tmux --with-utf8proc

#-------------------------------------------------------------
# Emacs Installation
#-------------------------------------------------------------
brew tap d12frosted/emacs-plus
brew install emacs-plus

ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications

brew services start d12frosted/emacs-plus/emacs-plus

emacs_version="$(emacs --version | head -1 | sed -E 's/.+\s([0-9]+\.[0-9]+)$/\1/')"
tic -o ~/.terminfo "/usr/local/share/emacs/$emacs_version/etc/e/eterm-color.ti"

#-------------------------------------------------------------
# MacVim Installation
# - Take advantage of MacVim's faster rendering engine
#-------------------------------------------------------------
# Note: Ensure everything is compiled against non-system Python

# # Use the MacVim binary as CLI vim
# brew install macvim \
#   --with-override-system-vim \
#   --with-lua  \
#   --with-luajit \
#   --HEAD

# ln -s /usr/local/opt/macvim/MacVim.app /Applications
