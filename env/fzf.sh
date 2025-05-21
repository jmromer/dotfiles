if [[ -z "${XDG_DATA_HOME}" ]]; then
  echo "WARNING: XDG_DATA_HOME env var is not set in $0"
fi

export FZF_DIR="${XDG_DATA_HOME}/fzf"
export FZF_DEFAULT_OPTS="
  --no-multi
  --exact
  --tiebreak=index
  --color='bg:#1d1e20,bg+:#1d1e20,preview-bg:#1d1e20,border:#1d1e20'
  --bind='ctrl-f:preview-down'
  --bind='ctrl-b:preview-up'
"

export FZF_DEFAULT_COMMAND="fd --hidden --type f --exclude .git --exclude node_modules"
export FZF_CTRL_T_COMMAND="${FZF_DEFAULT_COMMAND}"
