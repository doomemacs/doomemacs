EMACS=emacs

all: clean autoloads

update:
	cask update

clean:
	@rm -rf init.elc init/*.elc elisp/*.elc core/*.elc
	@rm -rf auto-save-list recentf places ido.last async-bytecomp.log elpa

autoloads:
	@rm -rf core/autoloads.el
	@cask exec ${EMACS} -Q --batch --eval '(progn (setq generated-autoload-file "~/.emacs.d/core/autoloads.el") (update-directory-autoloads "~/.emacs.d/init" "~/.emacs.d/core" "~/.emacs.d/elisp"))'
