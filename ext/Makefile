CACHE_DIR="$(HOME)/.emacs.d/private/cache/`hostname`/`emacs --version | grep -o '2[0-9]\.[0-9]'`"
ELPA_DIR="$(HOME)/.emacs.d/.cask/`emacs --version | grep -o '2[0-9]\.[0-9]\.[0-9]'`/elpa"

LLVMV="3.8.0"
LLVM="clang+llvm-$(LLVMV)-x86_64-apple-darwin"

#

all: rust csharp js sh cc

rust:
	@echo "Installing Rust dependencies"
	git clone --depth 1 --recursive "https://github.com/rust-lang/rust.git" rust
	git clone --depth 1 --recursive "https://github.com/phildawes/racer.git" racer-src
	cd racer-src && cargo build --release
	mv racer-src/target/release/racer ./racer
	rm -rf racer-src

csharp:
	@echo "Installing C# dependencies"
	git clone --depth 1 --recursive "https://github.com/OmniSharp/omnisharp-server" omnisharp
	cd omnisharp && xbuild
	mv omnisharp/bin/Debug/OmniSharp.exe ./OmniSharp.exe
	rm -rf omnisharp

js:
	@echo "Installing js2-mode dependencies"
	npm -g install trepanjs tern

sh:
	@echo "Installing zsh/bash dependencies"
	brew install zshdb bashdb

cc:
	@echo "Installing irony-mode dependencies"
	[ -f "clang.tar.xz" ] || wget "http://llvm.org/releases/$(LLVMV)/$(LLVM).tar.xz" -O clang.tar.xz
	@if [ ! -d clang ]; then \
		tar -xzvf clang.tar.xz; \
		mv "$(LLVM)" clang; \
	fi
	@mkdir -p build; cd build && cmake -DCMAKE_INSTALL_PREFIX=$(CACHE_DIR)/irony/ \
		-DLIBCLANG_LIBRARY=~/.emacs.d/ext/clang/lib/libclang.dylib \
		-DLIBCLANG_INCLUDE_DIR=~/.emacs.d/ext/clang/include/ \
		$(ELPA_DIR)/irony-2*/server && \
		cmake --build . --use-stderr --config Release --target install
	install_name_tool -change @rpath/libclang.dylib /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/libclang.dylib $(CACHE_DIR)/irony/bin/irony-server
