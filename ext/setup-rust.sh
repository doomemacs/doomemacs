#!/usr/bin/env bash

cd ~/.emacs.d/ext
source ./VARS

#
echo "Setting up Rust"

git-repo "https://github.com/rust-lang/rust.git" "rust"
git-repo "https://github.com/phildawes/racer.git" "racer-src"

cd racer-src && cargo build --release
mv racer-src/target/release/racer ./racer
