EMACS=emacs
CACHE_DIR=""

all: install update

install: autoloads
	@$(EMACS) -Q --batch --eval '(setq use-package-always-ensure t)' -l init.el

update: autoloads
	@$(EMACS) -Q --batch -l init.el -f 'doom/packages-update'

autoloads:
	@$(EMACS) -Q --batch -l init.el -f 'doom/refresh-autoloads'

compile:
	@$(EMACS) -Q --batch -l init.el -f 'doom/byte-compile'

clean: clean-elc
	@$(EMACS) -Q --batch -l init.el -f 'doom/packages-clean'

clean-elc:
	@rm -fv init.elc
	@find {core,modules} -type f -iname '*.elc' -exec rm \-fv {} \;

.PHONY: all
