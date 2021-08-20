#-------------------------------------------------------------
# Use Ripgrep for FZF instead of find
#-------------------------------------------------------------
if [[ -z "${XDG_DATA_HOME}" ]]; then
  echo "WARNING: XDG_DATA_HOME env var is not set in $0"
fi

# FZF_DEFAULT_COMMAND
# FZF_CTRL_T_COMMAND
export FZF_DIR="${XDG_DATA_HOME}/fzf"
export FZF_DEFAULT_OPTS="--no-multi --tiebreak=index --bind='ctrl-f:preview-down' --bind='ctrl-b:preview-up'"
