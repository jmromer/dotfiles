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
  fancy_echo "Installing ruby gem: $gem"
  npm install -g $gem
done

#-------------------------------------------------------------
# Node Packages
#-------------------------------------------------------------
node_packages=(
  ghost   # blogging platform
  n       # manage node versions
  jscs    # javascript style checker
  jshint  # js linter
  jsctags # ctags for js
)

for package in ${node_packages[*]}; do
  fancy_echo "Installing node package: $package..."
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
