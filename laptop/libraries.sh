#-------------------------------------------------------------
# Ruby gems
#-------------------------------------------------------------
ruby_gems=(
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
