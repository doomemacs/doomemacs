# Ensure emacs always runs from this makefile's PWD
EMACS = emacs -q --eval "(setq user-emacs-directory default-directory load-prefer-newer t)"
DOOM  = $(EMACS) --batch -l init.el
DOOMI = $(subst --batch,,$(DOOM))

MODULES = $(patsubst modules/%/, %, $(sort $(dir $(wildcard modules/*/ modules/*/*/))))

all: | autoloads autoremove install

## Shortcuts
a: autoloads
i: install
u: update
U: upgrade
r: autoremove
c: compile
cc: compile-core
ce: compile-elpa
d: doctor

quickstart: | ~/.doom.d/init.el install
~/.doom.d/init.el:
	mkdir ~/.doom.d && cp init.example.el ~/.doom.d/init.el

## Package management
install: | .local/autoloads.el
	@$(DOOM) -f doom//packages-install

update: | .local/autoloads.el
	@$(DOOM) -f doom//packages-update

autoremove: | .local/autoloads.el
	@$(DOOM) -f doom//packages-autoremove

autoloads:
	@$(DOOM) -f doom//reload-autoloads

upgrade: | _upgrade recompile all
_upgrade:
	@git pull origin $(shell git rev-parse --abbrev-ref HEAD)

## Byte compilation
# compile
# compile-core
# compile-module
# compile-module/submodule
compile: | clean
	@$(DOOM) -f doom//byte-compile

compile-core: | clean
	@$(DOOM) -f doom//byte-compile-core

compile-elpa:
	@$(DOOM) -f doom//byte-recompile-plugins

$(patsubst %, compile-%, $(MODULES)): | .local/autoloads.el
	@$(DOOM) -f doom//byte-compile -- $(patsubst compile-%, %, $@)

recompile:
	@$(DOOM) -f doom//byte-compile -- -r

clean:
	@$(DOOM) -f doom//clean-byte-compiled-files


## Unit tests
# test
# test-core
# test-module
# test-module/submodule
test: | .local/autoloads.el
	@$(DOOM) -f doom//run-tests

test-core $(patsubst %, test-%, $(MODULES)): | .local/autoloads.el
	@$(DOOM) -f doom//run-tests -- $(subst test-, , $@)

# run tests interactively
testi: | .local/autoloads.el
	@$(DOOMI) -f doom//run-tests


## Utility tasks
# Runs Emacs from a different folder than ~/.emacs.d; only use this for testing!
run:
	@$(DOOMI) $(ARGS) --eval "(run-hooks 'after-init-hook 'emacs-startup-hook 'window-setup-hook)"

# Diagnoses potential OS/environment issues
doctor:
	@$(EMACS) --script bin/doom-doctor

# Prints debug info about your current setup
info:
	@$(EMACS) --batch -l core/core.el -l core/autoload/debug.el -f doom/info

## Internal tasks
.local/autoloads.el:
	@$(DOOM) -f doom-initialize-autoloads

.PHONY: all compile test testi clean
