#!/usr/bin/env bash

#-------------------------------------------------------------
# Homebrewed GNU Tools
#-------------------------------------------------------------
# a collection of binary tools
brew install binutils
# core unix tools like ls, etc
brew install coreutils
# diff, cmp, diff3, sdiff
brew install diffutils
# find, locate, updatedb, xargs
brew install findutils --with-default-names
# awk: Pattern replacement
brew install gawk
# indent: For beautifying C code
brew install gnu-indent --with-default-names
# sed: Filtering text
brew install gnu-sed --with-default-names
# tar: Creating compressed archives
brew install gnu-tar --with-default-names
# which: Find location of given executable
brew install gnu-which --with-default-names
# TLS: Secure comms
brew install gnutls --with-default-names
# grep: find stuff
brew install grep --with-default-names
# watch files/dirs for changes
brew install watch
# word-by-word diffing
brew install wdiff --with-gettext
# easy downloads
brew install wget
