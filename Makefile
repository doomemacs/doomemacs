EMACS=emacs

all: update

update:
	git pull
	@cask install
	@cask update

clean: clean-extras
	@rm -f *.elc {core,modules,private,contrib}/*.elc {core,modules}/lib/*.elc

clean-extras:
	@rm -rf auto-save-list recentf places ido.last async-bytecomp.log elpa projectile-bookmarks.eld projectile.cache company-statistics-cache.el tramp smex-items semanticdb

autoloads:
	@emacs --script scripts/generate-autoloads.el

compile: autoloads
	@emacs --script scripts/byte-compile.el
