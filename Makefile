EMACS=emacs

all: update

update: autoloads
	@echo "Updating repo"
	@git pull | sed 's/^/  /'
	@cask install --verbose
	@echo "Updating outdated plugins"
	@cask outdated | sed 's/^/  /'
	@cask update --verbose
	@echo "Compiling certain scripts"
	@emacs -Q --batch -f batch-byte-compile init.el init-load-path.el core/core.el core/core-os-osx.el contrib/*.el

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
