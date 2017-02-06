EMACS=emacs

all: install update autoloads

install: init.el
	@$(EMACS) --batch -l core/core.el -f 'doom/packages-install'
	@$(EMACSCLIENT) -e '(doom/reload)' >/dev/null 2>&1 || true

update: init.el
	@$(EMACS) --batch -l core/core.el -f 'doom/packages-update'
	@$(EMACSCLIENT) -e '(doom/reload)' >/dev/null 2>&1 || true

clean: init.el
	@$(EMACS) --batch -l core/core.el -f 'doom/packages-autoremove'
	@$(EMACSCLIENT) -e '(doom/reload)' >/dev/null 2>&1 || true

autoloads: init.el
	@$(EMACS) --batch -l init.el -f 'doom/refresh-autoloads'
	@$(EMACSCLIENT) -e '(doom/reload)' >/dev/null 2>&1 || true

compile: init.el clean-elc
	@$(EMACS) --batch -l init.el -f 'doom/byte-compile'

compile-lite: init.el clean-elc
	@$(EMACS) --batch -l core/core.el --eval '(doom/byte-compile t)'

clean-cache:
	@$(EMACS) --batch -l core/core.el --eval '(delete-directory doom-cache-dir t)'

clean-elc:
	@rm -fv init.elc
	@find {core,modules} -type f -iname '*.elc' -exec rm \-fv {} \;

test:
	@$(EMACS) --batch -l test/init.el -f 'doom!run-tests'

init.el:
	@[ -f init.el ] || $(error No init.el file, please create one or copy init.example.el)

.PHONY: all test
