# ASDF configuration
# https://github.com/asdf-vm/asdf/blob/master/docs/manage/configuration.md

if [[ -z "${XDG_CONFIG_HOME}" ]]; then
  echo "WARNING: XDG_CONFIG_HOME env var is not set in ${0}"
fi

if [[ -z "${XDG_DATA_HOME}" ]]; then
  echo "WARNING: XDG_DATA_HOME env var is not set in ${0}"
fi

if [[ -z "${MACHINE_CORES}" ]]; then
  echo "WARNING: MACHINE_CORES env var is not set in ${0}"
fi

# Defaults to ~/.asdfrc. Can be set to any location.
export ASDF_CONFIG_FILE="${XDG_CONFIG_HOME}/asdf/config"

# The filename of the file storing the tool names and versions.
# Defaults to .tool-versions. Can be any valid filename.
export ASDF_DEFAULT_TOOL_VERSIONS_FILENAME="${XDG_CONFIG_HOME}/asdf/tool-versions"

# Defaults to ~/.asdf - Location of the asdf scripts.
# If you install asdf to some other directory, set this to that directory.
export ASDF_DIR="${XDG_DATA_HOME}/asdf-manager"
source "${ASDF_DIR}/asdf.sh"

# Defaults to ~/.asdf - Location where asdf install plugins, shims and installs.
# Can be set to any location before sourcing asdf.sh
export ASDF_DATA_DIR="${XDG_DATA_HOME}/asdf-versions"

# The number of cores number of cores to use when compiling the source code.
# Useful for setting `make -j`
export ASDF_CONCURRENCY="${MACHINE_CORES}"

# Default libraries locations
export ASDF_GEM_DEFAULT_PACKAGES_FILE="${XDG_CONFIG_HOME}/asdf/default-libraries-ruby"
export ASDF_NPM_DEFAULT_PACKAGES_FILE="${XDG_CONFIG_HOME}/asdf/default-libraries-nodejs"
export ASDF_PYTHON_DEFAULT_PACKAGES_FILE="${XDG_CONFIG_HOME}/asdf/default-libraries-python"
export ASDF_GOLANG_DEFAULT_PACKAGES_FILE="${XDG_CONFIG_HOME}/asdf/default-libraries-golang"

# extra config options for installs
export POSTGRES_EXTRA_CONFIGURE_OPTIONS="--with-uuid=e2fs"

