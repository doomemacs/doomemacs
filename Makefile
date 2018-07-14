DOOM = "bin/doom"
MODULES = $(patsubst modules/%/, %, $(sort $(dir $(wildcard modules/*/ modules/*/*/))))

all:
	@$(DOOM) refresh

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
