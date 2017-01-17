#!/usr/bin/env bash

source VARS

#
echo "Setting up Rust"

case "$OSTYPE" in
    darwin*)
        brew install rust
        ;;
    linux*)
        if is-arch; then
            sudo pacman --noconfirm -S rust cargo
        elif is-deb; then
            echo ...
            exit 1
        fi
esac

git-repo "https://github.com/rust-lang/rust.git" "rust"
git-repo "https://github.com/phildawes/racer.git" "racer-src"

cd racer-src && cargo build --release
mv racer-src/target/release/racer ./racer
