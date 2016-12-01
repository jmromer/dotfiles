#!/usr/bin/env bash

#-------------------------------------------------------------
# Generate tags for ruby stdlib
#-------------------------------------------------------------
mkdir -p ~/.rbenv/plugins
git clone git://github.com/tpope/rbenv-ctags.git  ~/.rbenv/plugins/rbenv-ctags
rbenv ctags

#-------------------------------------------------------------
# Update RubyGems and generate tags for gems
#-------------------------------------------------------------
gem install gem-ctags
rbenv rehash
gem ctags
