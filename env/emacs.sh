if [ -n "${INSIDE_EMACS}" ]; then
  export EDITOR=emacs
  export PRINT_COVERAGE=false
fi

export ORG_HOME="${HOME}/Writing/org"
