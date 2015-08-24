#-------------------------------------------------------------
# Ruby gems
#-------------------------------------------------------------
ruby_gems=(
  rubygems-update
  gem-ctags
  pry
  pry-byebug
  rake
  rspec
  rubocop
  tmuxinator
)

for gem in ${ruby_gems[*]}; do
  echo "Installing ruby gem: $gem" && echo
  gem install $gem
done

#-------------------------------------------------------------
# Node Packages
#-------------------------------------------------------------
node_packages=(
  babel
  babel-core
  babel-eslint
  babel-jscs
  bower
  chai
  coffeelint
  ember-cli
  eslint
  eslint-plugin-react
  express
  express-react-views
  generator-react-webpack
  ghost
  gifify
  google-closure-compiler
  grunt
  grunt-cli
  jscs
  jscs-jsdoc
  jsctags
  jsdoc
  learnyoureact
  mocha
  node-jsx
  npm
  react
)

for package in ${node_packages[*]}; do
  echo "Installing node package: $package..." && echo
  npm install -g $package
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
gem update --system
gem ctags
