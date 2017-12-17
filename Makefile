# Ensure emacs always runs from this makefile's PWD
EMACS_FLAGS=--eval '(setq user-emacs-directory default-directory)' -l core/core.el
EMACS=emacs --quick --batch $(EMACS_FLAGS)
EMACSI=emacs -q $(EMACS_FLAGS)

MODULES=$(patsubst modules/%, %, $(shell find modules/ -maxdepth 2 -type d))

all: autoloads autoremove install

## Shortcuts
a: autoloads
i: install
u: update
r: autoremove
c: compile
cc: compile-core
ce: compile-elpa

## Package management
install: init.el .local/autoloads.el
	@$(EMACS) -f doom//packages-install

update: init.el .local/autoloads.el
	@$(EMACS) -f doom//packages-update

autoremove: init.el .local/autoloads.el
	@$(EMACS) -f doom//packages-autoremove

autoloads: init.el
	@$(EMACS) -f doom//reload-autoloads


## Byte compilation
# compile
# compile-core
# compile-module
# compile-module/submodule
compile: init.el clean
	@$(EMACS) -f doom//byte-compile

compile-core: init.el clean
	@$(EMACS) -f doom//byte-compile-core

compile-elpa: init.el
	@$(EMACS) -f doom//byte-recompile-plugins

$(patsubst %, compile-%, $(MODULES)): init.el .local/autoloads.el
	@$(EMACS) -f doom//byte-compile -- $(patsubst compile-%, %, $@)

recompile: init.el
	@$(EMACS) -f doom//byte-compile -- -r

clean:
	@$(EMACS) -f doom//clean-byte-compiled-files


## Unit tests
# test
# test-core
# test-module
# test-module/submodule
test: init.el .local/autoloads.el
	@$(EMACS) -f doom//run-tests

test-core $(patsubst %, test-%, $(MODULES)): init.el .local/autoloads.el
	@$(EMACS) -f doom//run-tests -- $(subst test-, , $@)

# run tests interactively
testi: init.el .local/autoloads.el
	@$(EMACSI) -f doom//run-tests


## Utility tasks
# Runs Emacs from a different folder than ~/.emacs.d
run:
	@$(EMACSI) -l init.el

# Diagnoses potential OS/environment issues
doctor:
	@bin/doom-doctor

## Internal tasks
init.el:
	@$(error No init.el file; create one or copy init.example.el)

.local/autoloads.el:
	@$(EMACS) -f doom-initialize-autoloads

.PHONY: all compile test testi clean
