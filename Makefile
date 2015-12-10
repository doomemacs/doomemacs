EMACS=emacs

all: update

update: autoloads
	@echo "Updating repo"
	@git pull 2>&1 | sed 's/^/  /'
	@cask install --verbose 2>&1 | sed 's/^/  /'
	@echo "Updating outdated plugins"
	@cask outdated 2>&1 | sed 's/^/  /'
	@cask update --verbose 2>&1 | sed 's/^/  /'
	@echo "Compiling certain scripts"
	@emacs -Q --batch -f batch-byte-compile init-load-path.el core/core.el core/core-os-osx.el contrib/*.el 2>&1 | sed 's/^/  /'

clean: clean-extras clean-elc

clean-elc:
	@rm -f *.elc {core,modules,private,contrib}/*.elc {core,modules}/lib/*.elc

clean-extras:
	@rm -rf auto-save-list recentf places ido.last async-bytecomp.log elpa projectile-bookmarks.eld projectile.cache company-statistics-cache.el tramp smex-items semanticdb var anaconda-mode

autoloads:
	@echo "Generating autoloads"
	@emacs --script scripts/generate-autoloads.el 2>&1 | sed 's/^/  /'

compile: autoloads
	emacs --script scripts/byte-compile.el
