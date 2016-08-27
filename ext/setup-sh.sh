#!/usr/bin/env bash

cd ~/.emacs.d/ext
source ./VARS

#
echo "Setting up zsh/bash (zshdb, bashdb)"

if is-mac; then
    brew install zshdb bashdb
fi
