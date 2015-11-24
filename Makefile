EMACS=emacs

all: update

update: autoloads
	git pull
	cask install --verbose
	cask outdated
	cask update --verbose
	emacs -Q --batch -f batch-byte-compile init.el init-load-path.el core/core.el core/core-os-osx.el contrib/*.el

clean: clean-extras
	rm -f *.elc {core,modules,private,contrib}/*.elc {core,modules}/lib/*.elc

clean-extras:
	rm -rf auto-save-list recentf places ido.last async-bytecomp.log elpa projectile-bookmarks.eld projectile.cache company-statistics-cache.el tramp smex-items semanticdb var anaconda-mode

autoloads:
	emacs --script scripts/generate-autoloads.el

compile: autoloads
	emacs --script scripts/byte-compile.el
