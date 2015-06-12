EMACS=emacs

all: install update

install:
	@cask install

update: install autoloads
	@cask update

clean: clean-extras
	@rm -f init.elc {core,modules,private,contrib}/*.elc

clean-extras:
	@rm -rf auto-save-list recentf places ido.last async-bytecomp.log elpa projectile-bookmarks.eld projectile.cache company-statistics-cache.el tramp smex-items semanticdb

autoloads:
	@rm -rf core/autoloads.el
	@cask exec ${EMACS} --script scripts/generate-autoloads.el
