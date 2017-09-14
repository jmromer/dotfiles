#-------------------------------------------------------------
# Packages by language / framework
#-------------------------------------------------------------

# Install anaconda3
curl -o ~/Desktop/anaconda_install.sh \
     https://repo.continuum.io/archive/Anaconda3-4.4.0-MacOSX-x86_64.sh

# run installer
bash ~/Desktop/anaconda_install.sh

# Install Python packages for emacs
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
    yapf \

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

# Elixir
brew install exenv elixir-build erlang
exenv install 1.3.4
exenv global 1.3.4

# Haskell
brew cask install haskell-platform
cabal install apply-refact hlint stylish-haskell hasktags hoogle ghc-mod intero
