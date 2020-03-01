#!/usr/bin/env sh

# Open an org-capture popup frame from the shell. This opens a temporary emacs
# daemon if emacs isn't already running.
#
# Usage: org-capture [-k KEY] [MESSAGE]
# Examples:
#   org-capture -k n "To the mind that is still, the whole universe surrenders."

set -e

cleanup() {
  emacsclient --eval '(let (kill-emacs-hook) (kill-emacs))'
}

# If emacs isn't running, we start a temporary daemon, solely for this window.
if ! emacsclient --suppress-output --eval nil; then
  emacs --daemon
  trap cleanup EXIT INT TERM
  daemon=1
fi

# org-capture key mapped to argument flags
# keys=$(emacsclient -e "(+org-capture-available-keys)" | cut -d '"' -f2)
while getopts "hk:" opt; do
  key="\"$OPTARG\""
  break
done
shift $((OPTIND-1))

[ -t 0 ] && str="$*" || str=$(cat)

if [ $daemon ]; then
  emacsclient -a "" \
    -c -F '((name . "doom-capture") (width . 70) (height . 25) (transient . t))' \
    -e "(+org-capture/open-frame \"$str\" ${key:-nil})"
else
  # Non-daemon servers flicker a lot if frames are created from terminal, so we
  # do it internally instead.
  emacsclient -a "" \
    -e "(+org-capture/open-frame \"$str\" ${key:-nil})"
fi
