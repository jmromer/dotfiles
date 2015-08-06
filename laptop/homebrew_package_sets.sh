#-------------------------------------------------------------
# Packages by language / framework
#-------------------------------------------------------------
# Scala
brew cask install java
brew install scala
brew install sbt
brew cask install scala-ide
brew cask install intellij-idea-ce

# Clojure
brew install leiningen clojurescript

# Haskell
brew cask install ghc cabal-install

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

# R
brew tap homebrew/science
brew install r
brew cask install rstudio

# Julia
brew tap staticfloat/julia
brew install julia
