#-------------------------------------------------------------
# Homebrewed Packages
#-------------------------------------------------------------
homebrew=(
  awscli                  # AWS command line interface
  bash                    # Updated version of Bash
  bash-completion         # Command completions for Bash
  cmake                   # For YCM installation
  emacs                   # emacs duh
  git                     # Updated version of Git
  gtypist                 # Touch-type training
  hub                     # For github-flavored git
  mycli                   # MySQL CLI
  n                       # Node environment manager
  pgcli                   # Postgres CLI
  python                  # Python 2.7 + Pip
  rbenv-default-gems      # default gems to be installed
  source-highlight        # syntax highlighting for less
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

# brew install rcm (already installed)
brew install pick   # Fuzzy-select from standard out
brew install liftof # CLI for creating/configuring new Xcode projects
brew install parity # commands for keeping heroku envs in sync

#-------------------------------------------------------------
# Universal Ctags
#-------------------------------------------------------------
brew tap universal-ctags/universal-ctags

brew install --HEAD universal-ctags

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
