# frozen_string_literal: true

require "json"

# Template files access
class Template
  TEMPLATE_DIR = File.join(ENV.fetch("XDG_CONFIG_HOME"), "rails/template")

  attr_accessor :directory, :relative_path, :absolute_path, :basename

  def self.files(subdir = "")
    Dir
      .glob(File.join(TEMPLATE_DIR, subdir, "**/*"), File::FNM_DOTMATCH)
      .reject { |absolute_path| File.directory?(absolute_path) }
      .map { |absolute_path| absolute_path.delete_prefix("#{TEMPLATE_DIR}/") }
      .map { |relative_path| new(relative_path) }
      .reject { |template| template.basename == "Gemfile" }
  end

  def initialize(relative_path)
    self.relative_path = relative_path
    self.absolute_path = File.join(TEMPLATE_DIR, relative_path)
    self.basename = File.basename(absolute_path)
  end

  def contents
    File.read(absolute_path)
  end
end

# Clean up Gemfile
lines = File.readlines("Gemfile")
gemfile = lines.each_with_index.with_object([]) do |(line, i), acc|
  next acc << line unless line.match?(/^(?:# |\s+)*gem/)

  followed_by_blank = lines[i.next].strip.empty?
  followed_by_comment = lines[i.next.next]&.match?(/^\s*#/)
  lines.delete_at(i.next) if followed_by_blank && followed_by_comment
  acc << "#{line.chomp} #{acc.pop}"
end
File.open("Gemfile", "w") { |f| f.puts(gemfile) }

# Add gems to Gemfile
File.open("Gemfile", "a") do |file|
  file.puts Template.new("Gemfile").contents
end

# Add steps to setup script
lines = File.readlines("bin/setup")
setup = lines.each_with_index.with_object([]) do |(line, i), acc|
  next acc << line unless line.match?(/Preparing database/)

  db_path = ".db"
  db_init = <<~STR.strip.split("\n").map { "  #{_1}" }
    unless File.exist?("#{db_path}")
      puts "Initializing database..."
      system! "pg_ctl init -D #{db_path}"
    end
  STR
  acc.concat(db_init.push("\n"))

  db_start = <<~STR.strip.split("\n").map { "  #{_1}" }
    puts "Starting database server..."
    system "killall postgres"
    system "pg_ctl -D #{db_path} -l #{db_path}/log start"
    sleep 2
  STR
  acc.concat(db_start.push("\n"))

  acc << '  system("createuser -U postgres -s $USER 2>/dev/null")'
  acc << lines.delete_at(i.next)
end
File.open("bin/setup", "w") { |f| f.puts(setup) }

# Add template config files
#
# - Add gem configuration initializers
# - RSpec configuration files
# - Procfile
# - dotfiles
Template.files.each do |template|
  file(template.relative_path, template.contents)
end

after_bundle do
  # Generate spec/rails_helper.rb
  generate("rspec:install")
  run("yes | bin/rails generate bullet:install")

  # Prepend simplecov require to spec/rails_helper.rb
  rails_helper_lines = File.readlines("spec/rails_helper.rb")

  idx = rails_helper_lines.find_index { _1.match?(/Rails.root.glob.+support.+/) }
  if idx.nil?
    puts "WARNING: spec/support/ not found in spec/rails_helper.rb"
  else
    rails_helper_lines[idx] = <<~RB.strip
      Rails.root.glob("spec/support/**/*.rb").sort.each { |f| require f }
    RB
  end

  File.open("spec/rails_helper.rb", "w") do |file|
    file.puts(<<~RUBY)
      # require simplecov before anything else
      require_relative "./support/simplecov"

      #{rails_helper_lines.join}
    RUBY
  end

  run("bin/setup")
  run("bundle lock --add-platform ruby x86_64-linux x86_64-darwin-22 arm64-darwin-22")
  system("bin/format")

  git :init
  git add: "."
  git commit: "-m 'Initial commit'"

  run("bin/dev")
end
