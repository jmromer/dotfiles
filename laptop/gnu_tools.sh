#-------------------------------------------------------------
# Homebrewed GNU Tools and Vim
#-------------------------------------------------------------
brew tap homebrew/dupes

gnu_tools=(
  "binutils"                        # a collection of binary tools
  "coreutils"                       # core unix tools like ls, etc
  "diffutils"                       # diff, cmp, diff3, sdiff
  "findutils --with-default-names"  # find, locate, updatedb, xargs
  "gawk"                            # awk: Pattern replacement
  "gnu-indent --with-default-names" # indent: For beautifying C code
  "gnu-sed --with-default-names"    # sed: Filtering text
  "gnu-tar --with-default-names"    # tar: Creating compressed archives
  "gnu-which --with-default-names"  # which: Find location of given executable
  "gnutls --with-default-names"     # TLS: Secure comms
  "grep --with-default-names"       # grep: find stuff
  "macvim --with-client-server --override-system-vim"
  "watch"                           # watch files/dirs for changes
  "wdiff --with-gettext"            # word-by-word diffing
  "wget"                            # easy downloads
)

for gnu_tool in "${gnu_tools[@]}"; do
  fancy_echo "brew install $gnu_tool..."
  brew_install_or_upgrade $gnu_tool
done
