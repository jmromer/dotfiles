if [[ -z "${DOTFILES_DIR}" ]]; then
  echo "WARNING: DOTFILES_DIR env var is not set in ${0}"
fi

# user-specific data locations
export XDG_DATA_HOME="${DOTFILES_DIR}/share"     # data files (submodules)
export XDG_CONFIG_HOME="${DOTFILES_DIR}/config"  # configuration files (also set in LaunchAgent)
export XDG_STATE_HOME="${DOTFILES_DIR}/state"    # state files (logs, history, recently used files, current app state, etc)
export XDG_CACHE_HOME="${DOTFILES_DIR}/cache"    # ephemeral data files

export XDG_RUNTIME_DIR="${DOTFILES_DIR}/sockets" # ephemeral runtime files (sockets, named pipes, etc)
export XDG_LOCALS_DIR="${DOTFILES_DIR}/locals"   # un-synced, un-tracked dotfiles (not spec)
export XDG_SECURE_DIR="${DOTFILES_DIR}/secure"   # synced, tracked sensitive dotfiles (not spec)

# base directories to search for data files in addition to the $XDG_DATA_HOME base directory.
export XDG_DATA_DIRS="/usr/local/share/:/usr/share/"

# base directories to search for configuration files in addition to the $XDG_CONFIG_HOME base directory.
export XDG_CONFIG_DIRS="/etc/xdg"

export AWS_CONFIG_FILE="${XDG_SECURE_DIR}/config/aws/config"
export AWS_SHARED_CREDENTIALS_FILE="${XDG_SECURE_DIR}/config/aws/credentials"
export BUNDLE_DEFAULT_INSTALL_USES_PATH="${XDG_CACHE_HOME}/bundler"
export BUNDLE_USER_CACHE="${XDG_CACHE_HOME}/bundler"
export CUPS_CACHEDIR="${XDG_CACHE_HOME}/cups"
export CURLOPT_NETRC_FILE="${XDG_SECURE_DIR}/config/netrc"
export DOCKER_CONFIG="$XDG_CONFIG_HOME/docker"
export DOOMDIR="${XDG_CONFIG_HOME}/doom"
export GEMRC="${XDG_CONFIG_HOME}/gem/gemrc"
export GEM_HOME="${XDG_STATE_HOME}/gem"
export GEM_SPEC_CACHE="${XDG_CACHE_HOME}/gem"
export GNUPGHOME="${XDG_SECURE_DIR}/gnupg"
export GRIPHOME="${XDG_CONFIG_HOME}/grip"
export GTAGSCONF="${XDG_CONFIG_HOME}/gtags/gtags.conf"
export HEX_HOME="${XDG_STATE_HOME}/hex"
export INPUTRC="${XDG_CONFIG_HOME}/inputrc"
export IPYTHONDIR="${XDG_DATA_HOME}/ipython"
export JUPYTER_CONFIG_DIR="${XDG_CONFIG_HOME}/jupyter"
export LESSHISTFILE="${XDG_STATE_HOME}/less/history"
export LESSKEY="${XDG_CONFIG_HOME}/less/keys"
export NETRC="${XDG_SECURE_DIR}/config/netrc"
export NODE_REPL_HISTORY="${XDG_STATE_HOME}/node/repl_history"
export NPM_CONFIG_CACHE="${XDG_CACHE_HOME}/npm"
export NPM_CONFIG_DEVDIR="${XDG_CACHE_HOME}/npm/gyp"
export NPM_CONFIG_PREFIX="${XDG_DATA_HOME}/npm"
export NPM_CONFIG_USERCONFIG="${XDG_CONFIG_HOME}/npm/npmrc"
export PSQLRC="${XDG_CONFIG_HOME}/psql/psqlrc"
export REDISCLI_HISTFILE="${XDG_STATE_HOME}/redis/rediscli_history"
export RUBY_DEBUG_HISTORY_FILE="${XDG_STATE_HOME}/rdbg_history"
export SOLARGRAPH_CACHE="${XDG_CACHE_HOME}/solargraph"
export VIMINIT="source $XDG_CONFIG_HOME/vim/vimrc"
export WGETRC="${XDG_CONFIG_HOME}/wget/wgetrc"
export ZDOTDIR="${XDG_CONFIG_HOME}/zsh"
