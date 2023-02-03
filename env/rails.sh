if [ -n "${INSIDE_EMACS}" ]; then
  export DISABLE_SPRING=1
else
  export DISABLE_SPRING=0
fi

export RAILS_TEMPLATE="${XDG_CONFIG_HOME}/rails/template.rb"
