# ASDF configuration
# https://github.com/asdf-vm/asdf/blob/master/docs/manage/configuration.md

# Defaults to ~/.asdfrc. Can be set to any location.
export ASDF_CONFIG_FILE="$XDG_CONFIG_HOME/asdf/config"

# The filename of the file storing the tool names and versions.
# Defaults to .tool-versions. Can be any valid filename.
export ASDF_DEFAULT_TOOL_VERSIONS_FILENAME="$XDG_CONFIG_HOME/asdf/tool-versions"

# Defaults to ~/.asdf - Location of the asdf scripts. 
# If you install asdf to some other directory, set this to that directory.
export ASDF_DIR="$XDG_DATA_HOME/asdf-manager"

# Defaults to ~/.asdf - Location where asdf install plugins, shims and installs.
# Can be set to any location before sourcing asdf.sh
export ASDF_DATA_DIR="$XDG_DATA_HOME/asdf-versions"
