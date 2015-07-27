#-------------------------------------------------------------
# Ruby gems
#-------------------------------------------------------------
ruby_gems=(
  gem-ctags
  pry
  pry-byebug
  rake
  rspec
  rubocop
  tmuxinator
)

for gem in ${ruby_gems[*]}; do
  echo "Installing ruby gem: $gem"
  echo
  gem install $gem
done

#-------------------------------------------------------------
# Node Packages
#-------------------------------------------------------------
node_packages=(
  ghost   # blogging platform
  n       # manage node versions
  jscs    # javascript style checker
  eslint  # js linter
  jsctags # ctags for js
  grunt
  grunt-cli
  yo
)

for package in ${node_packages[*]}; do
  echo "Installing node package: $package..."
  echo
  npm install -g $package
done

#-------------------------------------------------------------
# Generate tags for ruby stdlib
#-------------------------------------------------------------
mkdir -p ~/.rbenv/plugins
git clone git://github.com/tpope/rbenv-ctags.git  ~/.rbenv/plugins/rbenv-ctags
rbenv ctags

#-------------------------------------------------------------
# Generate tags for gems
#-------------------------------------------------------------
gem ctags
