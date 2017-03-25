# Ensure emacs always runs from this makefile's PWD
EMACS_FLAGS=--eval "(setq user-emacs-directory default-directory)"
EMACS=emacs --batch $(EMACS_FLAGS) -l core/core.el


# Tasks
all: install update autoloads

install: init.el
	@$(EMACS) -f 'doom-initialize-autoloads' -f 'doom/packages-install'

update: init.el
	@$(EMACS) -f 'doom-initialize-autoloads' -f 'doom/packages-update'

autoremove: init.el
	@$(EMACS) -f 'doom-initialize-autoloads' -f 'doom/packages-autoremove'

autoloads: init.el
	@$(EMACS) -f 'doom/reload-autoloads'

compile: init.el clean
	@$(EMACS) -f 'doom/compile'

compile-lite: init.el clean
	@$(EMACS) -f 'doom/compile-lite'

clean:
	@$(EMACS) -f 'doom/clean-compiled'

clean-cache:
	@$(EMACS) -f 'doom/clean-cache'


# Syntactic sugar for bootstrapping modules. Allows: make bootstrap javascript
# See doom/bootstrap for more information.
ifeq (bootstrap,$(firstword $(MAKECMDGOALS)))
  ARGV := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  $(eval $(ARGV):;@:)
endif

bootstrap: init.el
	@$(EMACS) -f 'doom-initialize-autoloads' --eval "(doom/bootstrap '($(ARGV)))"


# This is only useful if your emacs.d is somewhere other than ~/.emacs.d (for
# development purposes for instance).
run:
	@emacs $(EMACS_FLAGS) -q -l init.el

init.el:
	@[ -e init.el ] || $(error No init.el file; create one or copy init.example.el)


.PHONY: all test bootstrap
