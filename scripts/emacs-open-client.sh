#!/bin/bash

# Path to your Emacs application
EMACS="/opt/homebrew/bin/emacs"
EMACS_CLIENT="/opt/homebrew/bin/emacsclient"

# Path should match `server-socket-dir` in emacs config
SERVER_SOCKET_PATH="${XDG_RUNTIME_DIR}/emacs/server"
DEBUG="1"

debug(){
 [[ -z "${DEBUG}" ]] && return
 echo "[EMACS] ${1}"
 syslog -s -l notice "[EMACS] ${1}"
}

main(){
 debug "Checking for server socket at ${SERVER_SOCKET_PATH}"
 socket_found="$(find "${SERVER_SOCKET_PATH}" -type s 2>/dev/null)"

 debug "Checking for open windows..."
 open_windows=$(emacs_has_open_windows)

 # TODO: figure out how to reliably check for an open window in the background
 # if [ -n "${socket_found}" ] && [ "${open_windows}" == "false" ]; then
 #   debug "Emacs server socket found but there no open windows. Removing socket."
 #   rm -f "${SERVER_SOCKET_PATH}"
 #   unset socket_found
 # fi

 if [ -n "${socket_found}" ]; then
   debug "Emacs server socket found at ${SERVER_SOCKET_PATH}"
 else
   debug "Emacs server socket not found at ${SERVER_SOCKET_PATH}"
   if pgrep -x "Emacs" > /dev/null; then
     debug "An Emacs process appears to be running."
   fi
   debug "Emacs server starting now."
   ${EMACS} --daemon
 fi

 if [[ "${open_windows}" == "true" ]]; then
   debug "Emacs has an open window on this desktop. Foregrounding it now."
 elif [[ "${open_windows}" == "false" ]]; then
   debug "Starting an Emacs client instance."
   start_emacs_client
 else
   debug "Unexpected output: ${open_windows}"
   start_emacs_client
 fi

 bring_emacs_to_front
}

start_emacs_client(){
 ${EMACS_CLIENT} \
   --no-wait \
   --create-frame \
   --frame-parameters "((left . 0.75) (top . 0) (width . 0.4) (fullscreen . fullheight))" \
   "$@"
 }

# Function to check if Emacs has any open windows
emacs_has_open_windows(){
 osascript -s o <<EOF
tell application "System Events"
	if (name of processes) does not contain "Emacs" then
		return false
	end if
	try
		tell application process "Emacs"
			do shell script "logger '[EMACS] Checking for open Emacs windows'"
			do shell script "logger '[EMACS] '" & quoted form of ("count: " & (count of windows))
			if (count of windows) > 0 then
				return true
			else
				return false
			end if
		end tell
	on error errMsg number errNum
		do shell script "logger '[EMACS] [Error]: '" & quoted form of (errMsg & " number: " & errNum)
		return false
	end try
	return -1
end tell
EOF
}

bring_emacs_to_front(){
 osascript <<EOF
 tell application "System Events"
   tell application process "Emacs"
     set frontmost to true
   end tell
 end tell
EOF
}

main

