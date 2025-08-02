#!/bin/bash

/Applications/Alacritty.app/Contents/MacOS/alacritty \
  --config-file "${XDG_CONFIG_HOME}/alacritty/btop.toml" \
  --command btop
