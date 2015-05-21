EMACS=emacs

all: update

update:
	cask update

clean:
	@rm -rf init.elc init/*.elc contrib/*.elc
	@rm -rf auto-save-list recentf places ido.last async-bytecomp.log elpa projectile-bookmarks.eld projectile.cache company-statistics-cache.el tramp smex-items

autoloads:
	@rm -rf init/autoloads.el
	@cask exec ${EMACS} -Q --batch --eval '(progn (setq generated-autoload-file "~/.emacs.d/init/autoloads.el") (update-directory-autoloads "~/.emacs.d/init" "~/.emacs.d/contrib"))'
