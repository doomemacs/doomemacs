EMACS=emacs
CACHE_DIR="private/cache/`hostname`/`emacs --version | grep -o '2[0-9]\.[0-9]'`"
REPO_URL="https://github.com/hlissner"

all: install autoloads init.elc

# If you run either of these with emacs open, run doom-reload afterwards
install: autoloads _install init.elc
update: autoloads _update init.elc

autoloads:
	@$(EMACS) --batch -l init.el --eval '(doom-reload-autoloads)' 2>&1

snippets:
	@[ -d private/snippets ] || git clone $(REPO_URL)/emacs-snippets private/snippets

clean:
	@rm -rf auto-save-list recentf places ido.last async-bytecomp.log elpa tramp
	@rm -rf projectile-bookmarks.eld projectile.cache company-statistics-cache.el
	@rm -rf var semanticdb anaconda-mode
	@rm -f *.elc {core,modules,private,contrib}/*.elc {core,modules}/defuns/*.elc

clean-cache:
	@find $(CACHE_DIR) -type f -maxdepth 1 -delete
	@rm -f $(CACHE_DIR)/{workgroups,pcache,ltxpng,backup}/*

########################################

%.elc: %.el
	@$(EMACS) --batch -l init.el -f batch-byte-compile 2>&1 $<

_update:
	@cask update 2>&1
	@rm -f init.elc

_install:
	@cask install 2>&1
	@rm -f init.elc
	@mkdir -p $(CACHE_DIR)/{undo,backup,workgroups}

.PHONY: all
