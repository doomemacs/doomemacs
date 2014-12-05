EMACS=emacs

all: clean compile

clean:
	@rm -rf init.elc init/*.elc elisp/*.elc
	@rm -rf auto-save-list recentf places ido.last async-bytecomp.log

compile:
	${EMACS} -Q --batch -L . -f batch-byte-compile init.el init/*.el elisp/*.el
	@rm -rf init/autoload.*
	${EMACS} -Q --batch -L . -f update-directory-autoloads init elisp
