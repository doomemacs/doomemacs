EMACS=emacs

all: install update autoloads

install: init.el
	@$(EMACS) --batch -l core/core.el -f 'doom/packages-install'

update: init.el
	@$(EMACS) --batch -l core/core.el -f 'doom/packages-update'

clean: init.el
	@$(EMACS) --batch -l core/core.el -f 'doom/packages-autoremove'

autoloads: init.el
	@$(EMACS) --batch -l core/core.el -f 'doom/refresh-autoloads'

compile: init.el clean-elc
	@$(EMACS) --batch -l core/core.el -f 'doom/byte-compile'

compile-all: init.el clean-elc
	@$(EMACS) --batch -l core/core.el --eval '(doom/byte-compile t)'

clean-cache:
	@$(EMACS) --batch -l core/core.el --eval '(delete-directory doom-cache-dir t)'

clean-elc:
	@rm -fv init.elc
	@find {core,modules} -type f -iname '*.elc' -exec rm \-fv {} \;

init.el:
	@[ -f init.el ] || $(error No init.el file, please create one or copy init.example.el)

.PHONY: all
