# common_config.sh
# configuration scripts common to both zsh and bash

# API keys, etc
source $HOME/.env.secure.sh

# environment variables, ruby manager
source $SHELL_CONFIG/env.sh

# mostly aliases
source $SHELL_CONFIG/aliases.sh

# mostly functions
source $SHELL_CONFIG/functions.sh

# git-related
source $SHELL_CONFIG/git.sh

