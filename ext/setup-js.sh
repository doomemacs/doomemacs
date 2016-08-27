#!/usr/bin/env bash

cd ~/.emacs.d/ext
source ./VARS

#
echo "Setting up JS (tern/trepanjs)"

if is-mac; then
    brew install node
fi

npm -g install trepanjs tern
