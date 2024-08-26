#!/bin/bash

# Path to your Emacs application
emacs_app="/opt/homebrew/bin/emacs"
emacsclient="/opt/homebrew/bin/emacsclient"

open_emacs_client() {
  # Check if the Emacs server is running
  if ! pgrep -x "Emacs" > /dev/null; then
    echo "Starting Emacs server..."
    $emacs_app --daemon
  fi

  if [ "$(emacs_has_open_windows)" = "true" ]; then
    bring_emacs_to_front
    exit 0
  fi

  # Use emacsclient to open the file
  # $emacsclient -c -n "$@"
  $emacsclient \
    --no-wait \
    --create-frame \
    --frame-parameters "((left . 0.75) (top . 0) (width . 0.4) (fullscreen . fullheight))" \
    "$@"
}

# Function to check if Emacs has any open windows
emacs_has_open_windows() {
    osascript <<EOF
      tell application "System Events"
        if (name of processes) does not contain "Emacs" then
          return false
        end if
        tell application process "Emacs"
          return (count of windows) > 0
        end tell
      end tell
EOF
}

bring_emacs_to_front() {
  osascript <<EOF
  tell application "System Events"
    tell application process "Emacs"
      set frontmost to true
    end tell
  end tell
EOF
}

open_emacs_client

