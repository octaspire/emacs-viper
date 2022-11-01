#!/usr/bin/env bash

FILE="$HOME"/.bashrc

if ! tail -n 5 "$FILE" | grep MACOS_PATH_HELPER_PROVIDED; then
  echo '# MACOS_PATH_HELPER_PROVISION' >> "$FILE"
  echo '# Get correct PATH value into GUI emacs that is not started from command line,' >> "$FILE"
  echo '# without using exec-path-from-shell package.' >> "$FILE"
  echo 'PATH_HELPER=/usr/libexec/path_helper' >> "$FILE"
  echo 'if [ -x "$PATH_HELPER" ]; then' >> "$FILE"
  echo '  eval `"$PATH_HELPER" -s`' >> "$FILE"
  echo '  defaults write "$HOME"/.MacOSX/environment PATH "$PATH"' >> "$FILE"
  echo 'fi' >> "$FILE"
  echo '# MACOS_PATH_HELPER_PROVIDED' >> "$FILE"
  echo "" >> "$FILE"
fi

