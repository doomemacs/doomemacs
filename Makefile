DOOM = "bin/doom"
MODULES = $(patsubst modules/%/, %, $(sort $(dir $(wildcard modules/*/ modules/*/*/))))

all: deprecated
	@$(DOOM) refresh

deprecated:
	@echo "Using make to manage your Doom config is deprecated"
	@echo
	@echo "Use the 'bin/doom' script instead. The equivalent of 'make' is 'doom refresh'."
	@echo
	@echo "See 'doom help' for a list of commands"
	@echo
	@read -p "Press enter to continue"

## Shortcuts
a: autoloads
i: install
u: update
U: upgrade
r: autoremove
c: compile
cc: compile-core
cp: compile-plugins
re: recompile
d: doctor

quickstart: install


## Package management
install: deprecated
	@$(DOOM) install
update: deprecated
	@$(DOOM) update
autoremove: deprecated
	@$(DOOM) autoremove
autoloads: deprecated
	@$(DOOM) autoloads
upgrade: deprecated
	@$(DOOM) upgrade

## Byte compilation
compile: deprecated
	@$(DOOM) compile
compile-core: deprecated
	@$(DOOM) compile :core
compile-private: deprecated
	@$(DOOM) compile :private
compile-plugins: deprecated
	@$(DOOM) build
recompile: deprecated
	@$(DOOM) recompile
clean: deprecated
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


## Utility tasks
# Runs Emacs from a different folder than ~/.emacs.d; only use this for testing!
run:
	@$(DOOM) run $(ARGS)
# Prints debug info about your current setup
info:
	@$(DOOM) info

# Diagnoses potential OS/environment issues
doctor:
	@$(DOOM) doctor

.PHONY: all compile test testi clean
