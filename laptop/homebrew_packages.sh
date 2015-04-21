#-------------------------------------------------------------
# Homebrewed Packages
#-------------------------------------------------------------
brew tap thoughtbot/formulae

homebrew=(
  awscli                  # AWS command line interface
  bash                    # Updated version of Bash
  bash-completion         # Command completions for Bash
  cmake                   # For YCM installation
  emacs                   # emacs duh
  fzf                     # fuzzy finder
  git                     # Updated version of Git
  gtypist                 # Touch-type training
  hub                     # For github-flavored git
  mongodb                 # document-based NoSQL database
  pick                    # for fuzzy-searching stdout
  rbenv-default-gems      # default gems to be installed
  source-highlight        # syntax highlighting for less
  tree                    # for viewing directory contents in tree format
  zsh                     # Updated version of Zshell
  zsh-completions         # Command completions for Zshell
  zsh-syntax-highlighting # Syntax highlighting as you type
)

for package in ${homebrew[*]}; do
  fancy_echo "Installing or upgrading $package..."
  brew_install_or_upgrade $package
done

