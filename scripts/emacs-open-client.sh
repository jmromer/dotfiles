#!/bin/bash

# Path to your Emacs application
EMACS="/opt/homebrew/bin/emacs"
EMACS_CLIENT="/opt/homebrew/bin/emacsclient"

# Path should match `server-socket-dir` in emacs config
SERVER_SOCKET_PATH="${XDG_RUNTIME_DIR}/emacs/server"
DEBUG="1"

debug() {
  [[ -z "${DEBUG}" ]] && return
  echo "[EMACS] ${1}"
  syslog -s -l notice "[EMACS] ${1}"
}

main() {
  if [ -S "${SERVER_SOCKET_PATH}" ]; then
    debug "Emacs server socket found at ${SERVER_SOCKET_PATH}"
  else
    debug "Emacs server socket not found at ${SERVER_SOCKET_PATH}"
    if pgrep -x "Emacs" > /dev/null; then
      debug "An Emacs process appears to be running."
    fi
    debug "Emacs server starting now."
    ${EMACS} --daemon
  fi

  debug "Checking for open windows..."
  result=$(emacs_has_open_windows)
  if [[ "${result}" == "true" ]]; then
    debug "Emacs has an open window on this desktop. Foregrounding it now."
  elif [[ "${result}" == "false" ]]; then
    debug "None found. Starting an Emacs client instance."
    start_emacs_client
  else
    debug "Unexpected output: ${result}"
  fi

  bring_emacs_to_front
}

start_emacs_client() {
  # Use emacsclient to open the file
  # $emacsclient -c -n "$@"
  ${EMACS_CLIENT} \
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
          if (count of windows) > 0
            return true
          else
            return false
          end if
        end tell
        return -1
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

main

