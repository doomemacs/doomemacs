#!/usr/bin/env bash

cd ~/.emacs.d/ext
source ./VARS

LLVMV="3.8.0"
LLVM="clang+llvm-${LLVMV}-x86_64-apple-darwin"

#
echo "Setting up C/C++ (irony-mode)"

if is-mac; then
    brew install cmake
	brew install llvm --with-clang
fi

# Build irony-server
git-repo "https://github.com/Sarcasm/irony-mode" "irony-mode"
cd irony-mode/server
[ -d build ] && rm -rf build
mkdir build && cd build
cmake -DCMAKE_INSTALL_RPATH_USE_LINK_PATH\=ON \
    -DCMAKE_INSTALL_PREFIX\=${CACHE_DIR}/irony/ ../ && \
    cmake --build . --use-stderr --config Release --target install
install_name_tool -change @rpath/libclang.dylib \
    /usr/local/opt/llvm/lib/libclang.dylib \
    ${CACHE_DIR}/irony/bin/irony-server
