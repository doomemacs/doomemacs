# Ensure emacs always runs from this makefile's PWD
EMACS=emacs --batch --eval "(setq user-emacs-directory default-directory)"


# Tasks
all: install update autoloads

install: init.el
	@$(EMACS) -l core/core.el -f 'doom/packages-install'

update: init.el
	@$(EMACS) -l core/core.el -f 'doom/packages-update'

autoremove: init.el
	@$(EMACS) -l core/core.el -f 'doom/packages-autoremove'

autoloads: init.el
	@$(EMACS) -l core/core.el -f 'doom/reload-autoloads'

compile: init.el clean
	@$(EMACS) -l core/core.el -f 'doom/recompile'

compile-lite: init.el clean
	@$(EMACS) -l core/core.el --eval '(doom/recompile t)'

clean:
	@rm -fv init.elc
	@find {core,modules} -type f -iname '*.elc' -exec rm \-fv {} \;

clean-cache:
	@$(EMACS) -l core/core.el --eval '(delete-directory doom-cache-dir t)'

test:
	@$(EMACS) -l test/init.el -f 'doom-run-tests'

init.el:
	@[ -f init.el ] || $(error No init.el file; create one or copy init.example.el)

.PHONY: all test
