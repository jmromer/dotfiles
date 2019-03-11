#!/usr/bin/env bash

sudo apt-get update --fix-missing
sudo apt-get upgrade


# Install Emacs 26 GUI
sudo apt-get remove emacs
sudo apt-get remove emacs-gtk
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt-get update
sudo apt-get install emacs26

sudo apt-get install -y autoconf
sudo apt-get install -y automake
sudo apt-get install -y build-essential
sudo apt-get install -y bzip2
sudo apt-get install -y curl
sudo apt-get install -y file
sudo apt-get install -y fonts-firacode
sudo apt-get install -y g++
sudo apt-get install -y gcc
sudo apt-get install -y git
sudo apt-get install -y gnome-tweak-tool
sudo apt-get install -y hardinfo
sudo apt-get install -y keychain
sudo apt-get install -y libffi-dev
sudo apt-get install -y libgl1-mesa-dev
sudo apt-get install -y libinput-tools
sudo apt-get install -y libncurses-dev
sudo apt-get install -y libqt4-dev
sudo apt-get install -y libreadline-dev
sudo apt-get install -y libssl-dev
sudo apt-get install -y libtool
sudo apt-get install -y libx11-6
sudo apt-get install -y libx11-dev
sudo apt-get install -y libxdo-dev
sudo apt-get install -y libxi-dev
sudo apt-get install -y libxml2
sudo apt-get install -y libxml2-dev
sudo apt-get install -y libxmlsec1-dev
sudo apt-get install -y libxslt-dev
sudo apt-get install -y libxslt1-dev
sudo apt-get install -y libxtst-dev
sudo apt-get install -y libyaml-dev
sudo apt-get install -y linuxbrew-wrapper
sudo apt-get install -y maildir-utils
sudo apt-get install -y make
sudo apt-get install -y openssl
sudo apt-get install -y pkg-config
sudo apt-get install -y sqlite3
sudo apt-get install -y unixodbc-dev
sudo apt-get install -y xclip
sudo apt-get install -y xsel

sudo apt-get install -y tensorflow-cuda-latest

sudo apt-get autoclean
sudo apt-get autoremove
sudo apt-get clean
