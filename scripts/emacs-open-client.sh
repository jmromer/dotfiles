#!/bin/bash

# Generate a unique server name (using process ID and current epoch time)
server_name="emacs_$$_$(date +%s)"
server_socket_path="${XDG_RUNTIME_DIR}/emacs/$server_name"

emacs --daemon="$server_socket_path"

timeout=8
while [ $timeout -gt 0 ]; do
  if [ -S "$server_socket_path" ]; then
    echo "Daemon started successfully."
    break
  else
    echo "Waiting for daemon to start..."
    sleep 1
    timeout=$((timeout - 1))
  fi
done

emacsclient --create-frame --no-wait --socket-name="$server_socket_path"

# bring_emacs_to_front
osascript <<EOF
 tell application "System Events"
   tell application process "Emacs"
     set frontmost to true
   end tell
 end tell
EOF
