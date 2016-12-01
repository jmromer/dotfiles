#-------------------------------------------------------------
# Packages by language / framework
#-------------------------------------------------------------

# OCaml
brew install opam
opam install merlin utop ocp-indent
opam init
opam config setup -a

# Go
brew install go
mkdir $HOME/.go
mkdir -p $HOME/.go/src/github.com/user

# Clojure
brew install leiningen clojurescript

# # Scala
# brew install scala
# brew install sbt
#
# # Haskell
# brew cask install haskell-platform
#
# # Vagrant
# brew cask install virtualbox vagrant
#
# # R
# brew tap homebrew/science
# brew install r
# brew cask install rstudio
