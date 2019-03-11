#!/usr/bin/env bash

set -e

sudo apt-get update --fix-missing
sudo apt-get upgrade


# Install Emacs 26 GUI
sudo apt-get remove emacs
sudo apt-get remove emacs-gtk
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt-get update
sudo apt-get install emacs26

sudo apt-get install autoconf
sudo apt-get install automake
sudo apt-get install build-essential
sudo apt-get install bzip2
sudo apt-get install curl
sudo apt-get install file
sudo apt-get install fonts-firacode
sudo apt-get install g++
sudo apt-get install gcc
sudo apt-get install git
sudo apt-get install gnome-tweak-tool
sudo apt-get install hardinfo
sudo apt-get install keychain
sudo apt-get install libffi-dev
sudo apt-get install libgl1-mesa-dev
sudo apt-get install libinput-tools
sudo apt-get install libncurses-dev
sudo apt-get install libqt4-dev
sudo apt-get install libreadline-dev
sudo apt-get install libssl-dev
sudo apt-get install libtool
sudo apt-get install libx11-6
sudo apt-get install libx11-dev
sudo apt-get install libxdo-dev
sudo apt-get install libxi-dev
sudo apt-get install libxml2
sudo apt-get install libxml2-dev
sudo apt-get install libxmlsec1-dev
sudo apt-get install libxslt-dev
sudo apt-get install libxslt1-dev
sudo apt-get install libxtst-dev
sudo apt-get install libyaml-dev
sudo apt-get install linuxbrew-wrapper
sudo apt-get install maildir-utils
sudo apt-get install make
sudo apt-get install openssl
sudo apt-get install pkg-config
sudo apt-get install sqlite3
sudo apt-get install unixodbc-dev
sudo apt-get install xclip
sudo apt-get install xsel

sudo apt-get autoclean
sudo apt-get autoremove
sudo apt-get clean

# sudo apt-get install tensorflow-cuda-latest
# conda create --name tf_gpu tensorflow-gpu
