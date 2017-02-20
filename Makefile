# Ensure emacs always runs from this makefile's PWD
EMACS=emacs --batch --eval "(setq user-emacs-directory default-directory)"


# Tasks
all: install update autoloads

install: init.el .local/autoloads.el
	@$(EMACS) -l core/core.el -f 'doom/packages-install'

update: init.el .local/autoloads.el
	@$(EMACS) -l core/core.el -f 'doom/packages-update'

autoremove: init.el .local/autoloads.el
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

# This is only useful if your emacs.d is somewhere other than ~/.emacs.d (for
# development purposes for instance).
run:
	@emacs --eval "(setq user-emacs-directory default-directory)" -q -l init.el

init.el:
	@[ -f init.el ] || $(error No init.el file; create one or copy init.example.el)

.local/autoloads.el:
	@$(EMACS) -l core/core.el -f 'doom/reload-autoloads'

.PHONY: all test
