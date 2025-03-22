#!/usr/bin/env bash

# Replace this with your actual command
OUTPUT=$(grind status)

# Properly escape the output for osascript
ESCAPED_OUTPUT=$(echo "$COMMAND_OUTPUT" | sed 's/\"/\\\"/g')

# Remove any ANSI color sequences
CLEAN_OUTPUT=$(echo "$OUTPUT" | sed 's/\x1b\[[0-9;]*m//g')

# Extract the first number (total problems)
TOTAL_PROBLEMS=$(echo "$CLEAN_OUTPUT" | grep -o "There are [0-9]* problems" | grep -o "[0-9]*")

# Extract the second number (due problems)
DUE_PROBLEMS=$(echo "$CLEAN_OUTPUT" | grep -o "of which [0-9]* are due" | grep -o "[0-9]*")

if [[ -z "$TOTAL_PROBLEMS" || -z "$DUE_PROBLEMS" ]]; then
  exit 1
fi

# Now you can use these variables in your notification
NOTIFICATION_MESSAGE="Problems Completed: $TOTAL_PROBLEMS\nDue for Review: $DUE_PROBLEMS"

# log execution time
echo "$(date) : $DUE_PROBLEMS / $TOTAL_PROBLEMS"

if [ "$DUE_PROBLEMS" -eq 0 ]; then
  exit 0
fi

# Send notification using macOS notification system with proper escaping
osascript <<EOF
display notification "$NOTIFICATION_MESSAGE" with title "Grind Status"
EOF
