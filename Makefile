EMACS=emacs

all: install update

install: init.el clean-elc
	@$(EMACS) --batch \
		--eval '(setq doom-auto-install-p t)' \
		-l init.el \
		--eval '(message "%s" (if doom--packages "All done!" "Nothing to install"))'

update: init.el clean-elc
	@$(EMACS) --batch -l init.el -f 'doom/packages-update'

clean: init.el clean-elc
	@$(EMACS) --batch -l init.el -f 'doom/packages-clean'

compile: init.el clean-elc
	@$(EMACS) --batch -l init.el -f 'doom/byte-compile'

autoloads: init.el
	@$(EMACS) --batch -l init.el -f 'doom/refresh-autoloads'

clean-cache:
	@$(EMACS) --batch -l core/core.el --eval '(delete-directory doom-cache-dir t)'

clean-elc:
	@rm -fv init.elc
	@find {core,modules} -type f -iname '*.elc' -exec rm \-fv {} \;

init.el:
	@[ -f init.el ] || $(error No init.el file, please create one or copy init.example.el)

.PHONY: all
