#!/usr/bin/env bash

#-------------------------------------------------------------
# Apps managed by Homebrew Cask
#-------------------------------------------------------------
# for alternate versions (e.g. iterm2-beta)
brew tap caskroom/versions

# password manager (NB: don't use the app store version)
brew cask install 1password

# removes apps and related files
brew cask install appcleaner

# Quick Look viewers: Zip files
brew cask install betterzipql

# video conferencing
brew cask install blue-jeans-launcher

# vpn
brew cask install cloak

# web browser
brew cask install firefox

# controls screen brightness
brew cask install flux

# web browser
brew cask install google-chrome

# web browser
brew cask install google-chrome-canary

# Java SDK
brew cask install java

# trackpad gestures
brew cask install bettertouchtool

# remap cmd to esc, caps lock to ctrl
brew cask install karabiner-elements

# system-wide custom keymappings
brew cask install keyboard-maestro

# cmd + <space> ftw
brew cask install launchbar

# batch file renamer
brew cask install name-mangler

# PDF article manager
brew cask install papers

# Quick Look viewers: Syntax highlighting
brew cask install qlcolorcode

# Quick Look viewers: Display image size and res
brew cask install qlimagesize

# Quick Look Preview: Markdown
brew cask install qlmarkdown

# Quick Look Preview: Patch files
brew cask install qlprettypatch

# Quick Look Preview: Extension-less files
brew cask install qlstephen

# Quick Look Preview: CSV files
brew cask install quicklook-csv

# Quick Look Preview: JSON files
brew cask install quicklook-json

# Gitter
brew cask install gitter

# remote pairing
brew cask install screenhero

# lightweight PDF reader with auto-update
brew cask install skim

# Skype, duh.
brew cask install skype

# Quick Look Preview: Mac OS X Installer Packages
brew cask install suspicious-package

# torrent machine
brew cask install transmission

# video player
brew cask install vlc

# Quick Look Preview: webp images
brew cask install webpquicklook

# for graphical terminal output (R and Octave dependency)
brew cask install xquartz

# for mockups
brew cask install sketch

# window organization
brew cask install amethyst

# documentation browser
brew cask install dash
