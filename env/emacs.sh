if [ -n "${INSIDE_EMACS}" ]; then
  export EDITOR=emacs
  export COVERAGE=false
  unset PRINT_COVERAGE
fi

export ORG_HOME="${HOME}/Writing/org"
