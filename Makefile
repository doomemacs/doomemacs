EMACS=emacs

all: update

update: install autoloads
	cask update

install:
	cask install

compile: clean
	@cask exec ${EMACS} -f narf::byte-compile

clean: clean-extras
	@rm -rf init.elc init/*.elc contrib/*.elc core/*.elc

clean-extras:
	@rm -rf auto-save-list recentf places ido.last async-bytecomp.log elpa projectile-bookmarks.eld projectile.cache company-statistics-cache.el tramp smex-items semanticdb

autoloads:
	@rm -rf core/autoloads.el
	@cask exec ${EMACS} -Q --batch --eval $$'(progn (setq generated-autoload-file "~/.emacs.d/core/autoloads.el") (update-directory-autoloads "~/.emacs.d/init" "~/.emacs.d/core" "~/.emacs.d/contrib"))'
