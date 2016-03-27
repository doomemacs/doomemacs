
all: rust csharp

rust:
	echo "Installing Rust dependencies"
	git clone --depth 1 --recursive https://github.com/rust-lang/rust.git rust
	git clone --depth 1 --recursive https://github.com/phildawes/racer.git racer-src
	cd racer-src && cargo build --release
	mv racer-src/target/release/racer ./racer
	rm -rf racer-src

csharp:
	echo "Installing C# dependencies"
	git clone --depth 1 --recursive https://github.com/OmniSharp/omnisharp-server omnisharp
	cd omnisharp && xbuild
	mv omnisharp/bin/Debug/OmniSharp.exe ./OmniSharp.exe
	rm -rf omnisharp

