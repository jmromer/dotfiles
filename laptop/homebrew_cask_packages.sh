#-------------------------------------------------------------
# Apps managed by Homebrew Cask
#-------------------------------------------------------------
brew tap caskroom/versions  # for alternate versions

cask=(
  1password            # password manager (NB: don't use the app store version)
  appcleaner           # removes apps and related files
  atom                 # GUI editor, mainly for previewing markdown
  betterzipql          # Quick Look viewers: Zip files
  firefox              # web browser
  flux                 # controls screen brightness
  google-chrome        # web browser
  google-chrome-canary # web browser
  iterm2-beta          # terminal emulator
  java                 # Java SDK
  jitouch              # trackpad gestures
  karabiner            # remaps esc to cmd
  keyboard-maestro     # system-wide custom keymappings
  latexit              # For generating TeX output
  launchbar            # cmd + <space> ftw
  name-mangler         # batch file renamer
  nvalt                # notes
  papers               # PDF article manager
  qlcolorcode          # Quick Look viewers: Syntax highlighting
  qlimagesize          # Quick Look viewers: Display image size and res
  qlmarkdown           # Quick Look Preview: Markdown
  qlprettypatch        # Quick Look Preview: Patch files
  qlstephen            # Quick Look Preview: Extension-less files
  quicklook-csv        # Quick Look Preview: CSV files
  quicklook-json       # Quick Look Preview: JSON files
  slack                # Slack, duh.
  skim                 # lightweight PDF reader with good annotation tools
  skype                # Skype, duh.
  spectacle            # window resizing
  suspicious-package   # Quick Look Preview: Mac OS X Installer Packages
  tex-live-utility     # package manager for TeX
  transmission         # torrent machine
  vlc                  # video player
  webpquicklook        # Quick Look Preview: webp images
  xquartz              # for graphical terminal output (R and Octave dependency)
)

for app in ${cask[*]}; do
  echo "Installing $app ..." && echo
  brew cask install $app
done
