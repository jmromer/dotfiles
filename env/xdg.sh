if [[ -z "${DOTFILES_DIR}" ]]; then
  echo "WARNING: DOTFILES_DIR env var is not set in ${0}"
fi

# user-specific data locations
export XDG_DATA_HOME="${DOTFILES_DIR}/share"     # data files (submodules)
export XDG_CONFIG_HOME="${DOTFILES_DIR}/config"  # configuration files (also set in LaunchAgent)
export XDG_STATE_HOME="${DOTFILES_DIR}/state"    # state files (logs, history, recently used files, current app state, etc)
export XDG_CACHE_HOME="${DOTFILES_DIR}/cache"    # ephemeral data files

export XDG_RUNTIME_DIR="${DOTFILES_DIR}/sockets" # ephemeral runtime files (sockets, named pipes, etc)
export XDG_LOCALS_DIR="${DOTFILES_DIR}/locals"   # un-synced, un-tracked dotfiles
export XDG_SECURE_DIR="${DOTFILES_DIR}/secure"   # synced, tracked sensitive dotfiles

# base directories to search for data files in addition to the $XDG_DATA_HOME base directory.
export XDG_DATA_DIRS="/usr/local/share/:/usr/share/"

# base directories to search for configuration files in addition to the $XDG_CONFIG_HOME base directory.
export XDG_CONFIG_DIRS="/etc/xdg"

# zsh config and ephemeral files locations (also set in LaunchAgent)
export ZDOTDIR="${XDG_CONFIG_HOME}/zsh"

# vim config (ephemeral files config therein)
export VIMINIT="source $XDG_CONFIG_HOME/vim/vimrc"

# gnupg
export GNUPGHOME="${XDG_SECURE_DIR}/gnupg"

# less
export LESSHISTFILE="${XDG_DATA_HOME}/less/history"
export LESSKEY="${XDG_CONFIG_HOME}/less/keys"

# misc
export INPUTRC="${XDG_CONFIG_HOME}/inputrc"
export JUPYTER_CONFIG_DIR="${XDG_CONFIG_HOME}/jupyter"

