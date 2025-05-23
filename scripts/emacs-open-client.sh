#!/bin/bash

debug(){
 echo "[EMACS] ${1}"
 syslog -s -l notice "[EMACS] ${1}"
}

bring_emacs_to_front () {
  debug "Bringing Emacs to front"

  osascript <<EOF
    tell application "Emacs"
      activate
    end tell
EOF

  osascript <<EOF
    tell application "System Events"
      tell application process "Emacs"
        set frontmost to true
      end tell
    end tell
EOF
}

emacsclient \
  --quiet \
  --create-frame \
  --no-wait \
  --alternate-editor '' \
  --frame-parameters "((left . 0.75) (top . 0) (width . 0.4) (fullscreen . fullheight))"

bring_emacs_to_front
