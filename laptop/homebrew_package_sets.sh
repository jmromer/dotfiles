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
# Install Pyenv
curl -L https://raw.githubusercontent.com/yyuu/pyenv-installer/master/bin/pyenv-installer | bash

# Install Python 2.7
pyenv install 2.7.13
pyenv shell 2.7.13
pip install --upgrade \
  "jedi>=0.9.0" \
  "json-rpc>=1.8.1" \
  "service_factory>=0.1.5" \
  autoflake \
  flake8 \
  hy \
  pip \
  pygments \
  setuptools \
  vim-vint \

# Install Python3
pyenv install 3.6.0
pyenv global 3.6.0
pip install --upgrade \
  "jedi>=0.9.0" \
  "json-rpc>=1.8.1" \
  "service_factory>=0.1.5" \
  autoflake \
  flake8 \
  hy \
  pip \
  pygments \
  setuptools \
  vim-vint \

# Set up neovim virtualenvs
pyenv virtualenv 2.7.13 neovim2
pyenv virtualenv 3.6.0 neovim3
pyenv activate neovim2
pip install neovim flake8
pyenv activate neovim3
pip install neovim flake8

# Elixir
brew install exenv elixir-build erlang
exenv install 1.3.4
exenv global 1.3.4

# React / React Native
brew install watchman
npm install -g create-react-app react-native-cli webpack

# Google Closure
brew install closure-compiler closure-linter

# Haskell
brew cask install haskell-platform
cabal install apply-refact hlint stylish-haskell hasktags hoogle ghc-mod intero

# # Scala
# brew install scala
# brew install sbt
#
#
# # Vagrant
# brew cask install virtualbox vagrant
#
# # R
# brew tap homebrew/science
# brew install r
# brew cask install rstudio
