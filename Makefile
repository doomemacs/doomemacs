# Ensure emacs always runs from this makefile's PWD
EMACS=emacs --batch --eval "(setq user-emacs-directory default-directory)"


# Tasks
all: install update autoloads

install: init.el
	@$(EMACS) -l core/core.el -f 'doom-initialize-autoloads' -f 'doom/packages-install'

update: init.el
	@$(EMACS) -l core/core.el -f 'doom-initialize-autoloads' -f 'doom/packages-update'

autoremove: init.el
	@$(EMACS) -l core/core.el -f 'doom-initialize-autoloads' -f 'doom/packages-autoremove'

autoloads: init.el
	@$(EMACS) -l core/core.el -f 'doom/reload-autoloads'

compile: init.el clean
	@$(EMACS) -l core/core.el -f 'doom/recompile'

clean:
	@$(EMACS) -l core/core.el -f 'doom/clear-compiled'

clean-cache:
	@$(EMACS) -l core/core.el -f 'doom/clear-cache'

# This is only useful if your emacs.d is somewhere other than ~/.emacs.d (for
# development purposes for instance).
run:
	@emacs --eval "(setq user-emacs-directory default-directory)" -q -l init.el

init.el:
	@[ -e init.el ] || $(error No init.el file; create one or copy init.example.el)

.PHONY: all test
