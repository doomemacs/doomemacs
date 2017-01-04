#!/usr/bin/env bash

source VARS

#
echo "Setting up JS (tern/trepanjs)"

case "$OSTYPE" in
    darwin*)
        brew install node
        ;;
    linux*)
        if is-arch; then
            sudo pacman --needed --noconfirm -S nodejs npm
        else
            echo "..."
            exit 1
        fi
        ;;
esac

npm -g install trepanjs tern
