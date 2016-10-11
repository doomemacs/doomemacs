#!/usr/bin/env bash

source VARS

#
echo "Setting up zsh/bash (zshdb, bashdb)"

if is-mac; then
    brew install zshdb bashdb
elif is-arch; then
    sudo pacman --noconfirm -S zshdb bashdb
fi
