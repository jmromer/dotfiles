# frozen_string_literal: true

# Flipper (feature-flagging) configuration

return unless defined?(Flipper) && defined?(Redis)

Flipper.configure do |config|
  config.adapter { Flipper::Adapters::Redis.new(Redis.new) }
end

# Register actor groups
# Flipper.register(:admins) { |actor| actor.admin? }
# Flipper.register(:early_access) { |actor| actor.early_access? }
