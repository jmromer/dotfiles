#-------------------------------------------------------------
# Packages by language / framework
#-------------------------------------------------------------
# Scala
brew install scala sbt
brew cask install scala-ide

# Clojure
brew install leiningen clojurescript

# Haskell
brew cask install haskell-platform

# Vagrant
brew cask install virtualbox vagrant

# Python, Pip, Buster
brew install python
pip install --upgrade pip setuptools buster
brew unlink python

# Exercism
brew tap homebrew/binary
brew install exercism

# Go
brew install go
mkdir $HOME/.go
mkdir -p $HOME/.go/src/github.com/user

