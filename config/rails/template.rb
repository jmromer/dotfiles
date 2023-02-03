# frozen_string_literal: true

require "json"

class Template
  TEMPLATE_DIR = File.join(ENV["XDG_CONFIG_HOME"], "rails/template")

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

# Add docs generation step to setup script (edit manually)
File.open("bin/setup", "a") do |file|
  file.puts 'puts "\n== Building documentation =="', 'system! "yard gems"'
end

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
  generate("bullet:install")

  # Prepend simplecov require to spec/rails_helper.rb
  rails_helper_lines = File.readlines("spec/rails_helper.rb")

  idx = rails_helper_lines.find_index { _1.match?(/Dir.+spec.+support/) }
  rails_helper_lines[idx] = <<~RB.squish
    Dir[Rails.root.join("spec/support/**/*.rb")].each { require(_1) }
  RB

  idx = rails_helper_lines.find_index { _1.match?(/config.fixture_path = /) }
  config, = rails_helper_lines[idx].split(" = ")
  rails_helper_lines[idx] = <<~RB.squish
    #{config} = Rails.root.join("spec/fixtures")
  RB

  File.open("spec/rails_helper.rb", "w") do |file|
    file.puts(<<~RUBY)
      # require simplecov before anything else
      require_relative "./support/simplecov"

      #{rails_helper_lines.join}
    RUBY
  end

  run(<<~SH)
    rufo **/*.rb
    rubocop --autocorrect-all --fail-level F
    rubocop
  SH
end
