#!/usr/bin/env bash

source VARS

#
echo "Setting up C/C++ (irony-mode)"

case "$OSTYPE" in
    darwin*)
        brew install cmake
        brew install llvm --with-clang
        ;;
    linux*)
        if is-arch; then
            sudo pacman --noconfirm --needed -S cmake clang
        elif is-deb; then
            echo "Not implemented"
            exit 1
        fi
        ;;
esac

# Build irony-server
git-repo "https://github.com/Sarcasm/irony-mode" "irony-mode"

# Reset build directory
cd irony-mode/server
[ -d build ] && rm -rf build
mkdir build && cd build

DEST="$(pwd)/irony-mode/server/build/irony/"

# Compile
if is-mac
then
    cmake -DCMAKE_INSTALL_RPATH_USE_LINK_PATH\=ON \
          -DCMAKE_INSTALL_PREFIX\="$DEST" ../
else
    cmake -DCMAKE_INSTALL_PREFIX\="$DEST" ../
fi
cmake --build . --use-stderr --config Release --target install

if is-mac
then
    install_name_tool -change @rpath/libclang.dylib \
        /usr/local/opt/llvm/lib/libclang.dylib \
        $DEST/bin/irony-server
fi
