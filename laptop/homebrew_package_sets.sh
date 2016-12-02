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
go get -u -v github.com/nsf/gocode
go get -u -v github.com/rogpeppe/godef
go get -u -v golang.org/x/tools/cmd/guru
go get -u -v golang.org/x/tools/cmd/gorename
go get -u -v golang.org/x/tools/cmd/goimports

# Clojure
brew install leiningen clojurescript

# Python
pip3 install --upgrade flake8

# Elixir
brew install exenv elixir-build erlang
exenv install 1.3.4

# React / React Native
brew install watchman
npm install -g create-react-app react-native-cli webpack

# Google Closure
brew install closure-compiler closure-linter

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
