#!/usr/bin/env bash

#-------------------------------------------------------------
# Homebrewed GNU Tools
#-------------------------------------------------------------
brew tap homebrew/dupes

# a collection of binary tools
brew install --force binutils

# core unix tools like ls, etc
brew install --force coreutils

# diff, cmp, diff3, sdiff
brew install --force diffutils

# find, locate, updatedb, xargs
brew install --force findutils --with-default-names

# awk: Pattern replacement
brew install --force gawk

# indent: For beautifying C code
brew install --force gnu-indent --with-default-names

# sed: Filtering text
brew install --force gnu-sed --with-default-names

# tar: Creating compressed archives
brew install --force gnu-tar --with-default-names

# which: Find location of given executable
brew install --force gnu-which --with-default-names

# TLS: Secure comms
brew install --force gnutls --with-default-names

# grep: find stuff
brew install --force grep --with-default-names

# watch files/dirs for changes
brew install --force watch

# word-by-word diffing
brew install --force wdiff --with-gettext

# easy downloads
brew install --force wget
