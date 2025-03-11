#!/bin/bash

emacsclient \
  --quiet \
  --create-frame \
  --no-wait \
  --alternate-editor ''

# bring emacs to front
osascript <<EOF
  tell application "Emacs"
    activate
  end tell

  tell application "System Events"
    tell application process "Emacs"
      set frontmost to true
    end tell
  end tell
EOF
