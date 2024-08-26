#!/bin/bash

# Path to your Emacs application
emacs_app="/opt/homebrew/bin/emacs"
emacsclient="/opt/homebrew/bin/emacsclient"

# Check if the Emacs server is running
if ! pgrep -x "Emacs" > /dev/null; then
  echo "Starting Emacs server..."
  $emacs_app --daemon
fi

# Use emacsclient to open the file
# $emacsclient -c -n "$@"
$emacsclient \
  --no-wait \
  --create-frame \
  --frame-parameters "((left . 0.75) (top . 0) (width . 0.4) (fullscreen . fullheight))" \
  "$@"
