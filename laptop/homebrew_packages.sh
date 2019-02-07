#-------------------------------------------------------------
# Homebrewed Packages
#-------------------------------------------------------------
packages=(
  asdf
  awscli                  # AWS command line interface
  bash                    # Updated version of Bash
  bash-completion         # Command completions for Bash
  ccat                    # colorized cat
  cmake                   # For YCM installation
  colordiff
  direnv                  # directory-specific env settings
  elixir-build
  gpg2                    # for PGP commit signing
  pinentry-mac
  fd
  fzf
  hub                     # For github-flavored git
  hugo
  imagemagick
  ispell
  mps-youtube             # command-line music player
  mu                      # command-line email indexing
  isync                   # command-line email sync
  msmtp                   # command-line email delivery
  w3m                     # HTML -> text
  osxutils
  pgcli                   # Postgres CLI
  postgres
  qt
  shellcheck
  reattach-to-user-namespace
  redis
  ripgrep
  source-highlight        # syntax highlighting for less
  the_silver_searcher
  tree                    # for viewing directory contents in tree format
  zsh                     # Updated version of Zshell
  zsh-completions         # Command completions for Zshell
  zsh-syntax-highlighting # Syntax highlighting as you type
)

for package in ${packages[*]}; do
  echo "Installing or upgrading $package..." && echo
  brew install $package
done

#-------------------------------------------------------------
# fzf completions
#-------------------------------------------------------------
source "$(brew --prefix fzf)/install"

#-------------------------------------------------------------
# gitin
#-------------------------------------------------------------
brew tap isacikgoz/gitin
brew install gitin

#-------------------------------------------------------------
# Universal Ctags
#-------------------------------------------------------------
brew install --HEAD universal-ctags/universal-ctags/universal-ctags

#-------------------------------------------------------------
# GNU Global
#-------------------------------------------------------------
brew install global --with-ctags --with-pygments

#-------------------------------------------------------------
# Git
#-------------------------------------------------------------
brew install openssl libyaml libffi
brew install --with-openssl curl
brew install --with-curl --with-openssl git

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
brew install neovim/neovim/neovim
mkdir "$HOME/.config"
ln -s "$HOME/.vim" "$HOME/.config/nvim"

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
