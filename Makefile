# Ensure emacs always runs from this makefile's PWD
DOOM = bin/doom
EMACS = emacs -q $(ARGS) -l init.el

MODULES = $(patsubst modules/%/, %, $(sort $(dir $(wildcard modules/*/ modules/*/*/))))

all:
	@$(DOOM) reload

## Shortcuts
a: autoloads
i: install
u: update
U: upgrade
r: autoremove
c: compile
cc: compile-core
ce: compile-elpa
re: recompile
d: doctor

quickstart:
	@$(DOOM) quickstart


## Package management
install:
	@$(DOOM) install
update:
	@$(DOOM) update
autoremove:
	@$(DOOM) autoremove
autoloads:
	@$(DOOM) autoloads
upgrade:
	@$(DOOM) upgrade

## Byte compilation
compile:
	@$(DOOM) compile
compile-core:
	@$(DOOM) compile :core
compile-private:
	@$(DOOM) compile :private
compile-plugins:
	@$(DOOM) compile :plugins
recompile:
	@$(DOOM) recompile
clean:
	@$(DOOM) clean
# compile-module
# compile-module/submodule
$(patsubst %, compile-%, $(MODULES)): | .local/autoloads.el
	@$(DOOM) $@ $(subst compile-, , $@)


## Unit tests
test:
	@$(DOOM) test
test-core:
	@$(DOOM) test :core
# test-module
# test-module/submodule
$(patsubst %, test-%, $(MODULES)):
	@$(DOOM) test $(subst test-, , $@)
# run tests interactively
testi:
	@$(EMACS) -l core/autoload/doom.el -f doom//run-tests


## Utility tasks
# Runs Emacs from a different folder than ~/.emacs.d; only use this for testing!
run:
	@$(EMACS)
# Prints debug info about your current setup
info:
	@$(DOOM) info

# Diagnoses potential OS/environment issues
doctor:
	@$(DOOM) doctor

.PHONY: all compile test testi clean
