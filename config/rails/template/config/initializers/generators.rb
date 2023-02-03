# frozen_string_literal: true

Rails.application.config.generators do |g|
  g.orm :active_record
  g.stylesheets false
  g.template_engine :erb
  g.test_framework :rspec, view_specs: false, routing_specs: false
end
