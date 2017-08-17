# frozen_string_literal: true

gem_group :development, :test do
  gem "factory_girl_rails"
  gem "jazz_fingers"
  gem "pry-byebug"
  gem "pry-rails"
  gem "rspec-rails"
  gem "vcr"
  gem "webmock"
end

gem_group :test do
  gem "capybara"
  # gem "shoulda-matchers",
  #    github: "thoughtbot/shoulda-matchers",
  #    branch: "rails-5"
end

gem_group :development do
  gem "spring-commands-rspec"
end

run "rm README.md"
run "echo \"* $(basename $(pwd))\" >> README.org"

## add config initializer
initializer "generators.rb", <<~RUBY
  Rails.application.config.generators do |g|
    g.factory_girl false
    g.helper false
    g.javascripts false
    g.stylesheets false
    g.test_framework :rspec, view_specs: false, routing_specs: false
    g.template_engine nil
  end
RUBY
