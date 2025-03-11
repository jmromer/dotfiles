#!/bin/bash

# Generate a unique server name (using process ID and current epoch time)
server_name="emacs_$$_$(date +%s)"
server_socket_path="${XDG_RUNTIME_DIR}/emacs/$server_name"

emacs --daemon="$server_socket_path"

timeout=7
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

# if the timeout has been exhausted and still no socket, alert and exit
if [ ! -S "$server_socket_path" ] && [ $timeout -eq 0 ]; then
  osascript <<EOF
    display dialog "Failed to start Emacs daemon within timeout period."
    buttons {"OK"}
    default button "OK"
    with icon stop with title "Emacs Startup Error"
EOF
  exit 1
fi

# --frame-parameters "((left . 0.75) (top . 0) (width . 0.4) (fullscreen . fullheight))"
emacsclient --create-frame \
  --no-wait \
  --socket-name="$server_socket_path" \
  --frame-parameters "((fullscreen . maximized))"

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
