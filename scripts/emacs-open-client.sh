#!/bin/bash

bring_emacs_to_front(){
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
  --alternate-editor ''

bring_emacs_to_front
