EMACS=emacs

all: update

update: autoloads
	@echo "Updating repo"
	@git pull 2>&1 | sed 's/^/  /'
	@echo "Updating outdated plugins"
	@cask install --verbose 2>&1 | sed 's/^/  /'
	@cask update --verbose 2>&1 | sed 's/^/  /'
	@echo "Compiling certain scripts"
	@emacs -Q --batch -f batch-byte-compile init-packages.el core/core.el core/core-os-osx.el contrib/*.el 2>&1 | sed 's/^/  /'

clean: clean-files clean-elc

clean-files:
	@echo "Cleaning derelict emacs files"
	@rm -rf auto-save-list recentf places ido.last async-bytecomp.log elpa projectile-bookmarks.eld projectile.cache company-statistics-cache.el tramp smex-items
	@rm -rf var semanticdb anaconda-mode

clean-elc:
	@echo "Cleaning *.elc"
	@rm -f *.elc {core,modules,private,contrib}/*.elc {core,modules}/lib/*.elc

clean-wg:
	@echo "Removing default session"
	@rm -f "private/cache/`hostname`/`emacs --version | grep -o '2[0-9]\.[0-9]'`/wg-default"

autoloads:
	@echo "Generating autoloads"
	@emacs --script scripts/generate-autoloads.el 2>&1 | sed 's/^/  /'

compile: autoloads
	@echo "Byte-compiling .emacs.d"
	emacs --script scripts/byte-compile.el | sed 's/^/  /'
