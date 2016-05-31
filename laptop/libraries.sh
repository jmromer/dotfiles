#!/usr/bin/env bash

echo "Installing n and node..."
curl -L http://git.io/n-install | N_PREFIX=~/.node bash -s -- -y

#-------------------------------------------------------------
# Node Packages
#-------------------------------------------------------------
node_packages=(
  npm
  react
)

for package in ${node_packages[*]}; do
  echo "Installing node package: $package..." && echo
  npm install -g "$package"
done

#-------------------------------------------------------------
# Generate tags for ruby stdlib
#-------------------------------------------------------------
mkdir -p ~/.rbenv/plugins
git clone git://github.com/tpope/rbenv-ctags.git  ~/.rbenv/plugins/rbenv-ctags
rbenv ctags

#-------------------------------------------------------------
# Update RubyGems and generate tags for gems
#-------------------------------------------------------------
gem ctags
