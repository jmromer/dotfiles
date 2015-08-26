#-------------------------------------------------------------
# Packages by language / framework
#-------------------------------------------------------------
# Scala
brew install scala
brew install sbt
brew cask install scala-ide
brew cask install intellij-idea-ce

# Clojure
brew install leiningen clojurescript

# Haskell
brew cask install haskell-platform

# Vagrant
brew cask install virtualbox vagrant

# Docker
brew cask install docker-compose

# Python, Pip, Buster, NeoVim Python Client
pip install --upgrade pip setuptools
pip install --upgrade buster
pip install --upgrade neovim

# Exercism
brew tap homebrew/binary
brew install exercism

# Go
brew install go
mkdir $HOME/.go
mkdir -p $HOME/.go/src/github.com/user

# R
brew tap homebrew/science
brew install r
brew cask install rstudio

# Julia
brew tap staticfloat/julia
brew install julia
