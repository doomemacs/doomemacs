# Ensure emacs always runs from this makefile's PWD
EMACS_FLAGS=--eval '(setq user-emacs-directory default-directory)' -l core/core.el
EMACS=emacs --batch $(EMACS_FLAGS)
EMACSI=emacs -q $(EMACS_FLAGS)

MODULES=$(patsubst modules/%, %, $(shell find modules/ -maxdepth 2 -type d))

# Tasks
all: autoloads autoremove install

install: init.el .local/autoloads.el
	@$(EMACS) -f doom/packages-install

update: init.el .local/autoloads.el
	@$(EMACS) -f doom/packages-update

autoremove: init.el .local/autoloads.el
	@$(EMACS) -f doom/packages-autoremove

autoloads: init.el
	@$(EMACS) -f doom/reload-autoloads

recompile: init.el
	@$(EMACS) -f doom/recompile

compile: init.el clean
	@$(EMACS) -f doom/compile

core: init.el clean
	@$(EMACS) -f doom/compile -- init.el core

$(MODULES): init.el .local/autoloads.el
	@rm -fv $(shell find modules/$@ -type f -name '*.elc')
	@$(EMACS) -f doom/compile -- modules/$@

clean:
	@$(EMACS) -f doom/clean-compiled

clean-cache:
	@$(EMACS) -f doom/clean-cache

clean-pcache:
	@$(EMACS) -l persistent-soft --eval '(delete-directory pcache-directory t)'

test: init.el .local/autoloads.el
	@$(EMACS) -f doom-run-tests

test\:core $(patsubst %, test\:%, $(MODULES)): init.el .local/autoloads.el
	@$(EMACS) -f doom-run-tests -- $(subst test:, , $@)

# run tests interactively
testi: init.el .local/autoloads.el
	@$(EMACSI) -f doom-run-tests -f ert

# For running Emacs from a different folder than ~/.emacs.d
run:
	@$(EMACSI) $(EMACS_FLAGS) -l init.el

doctor:
	@./bin/doctor

#
init.el:
	@[ -e init.el ] || $(error No init.el file; create one or copy init.example.el)

.local/autoloads.el:
	@$(EMACS) -f doom-initialize-autoloads

%.elc: %.el
	@$(EMACS) -f doom/compile -- $<


.PHONY: all test $(MODULES)
