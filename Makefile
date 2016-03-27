EMACS=emacs
CACHE_DIR="private/cache/`hostname`/`emacs --version | grep -o '2[0-9]\.[0-9]'`"
REPO_URL="https://github.com/hlissner"

all: install autoloads bootstrap.elc

install: autoloads bootstrap.elc
	@cask install 2>&1

# If you keep emacs open while running this, run narf/reload afterwards
update: autoloads _update bootstrap.elc

autoloads:
	@$(EMACS) --script scripts/generate-autoloads.el 2>&1

compile: autoloads bootstrap.elc
	@$(EMACS) --batch -f batch-byte-compile 2>&1 {core,modules,modules/contrib,private}/*.el {core,modules}/defuns/*.el

snippets:
	@[ -d private/snippets ] || git clone $(REPO_URL)/emacs-snippets private/snippets


clean:
	@rm -rf auto-save-list recentf places ido.last async-bytecomp.log elpa tramp projectile-bookmarks.eld projectile.cache company-statistics-cache.el var semanticdb anaconda-mode
	@rm -f *.elc {core,modules,private,contrib}/*.elc {core,modules}/defuns/*.elc

reset:
	@find $(CACHE_DIR) -type f -maxdepth 1 -delete
	@rm -f $(CACHE_DIR)/{workgroups,pcache,ltxpng,backup}/*


%.elc: %.el
	@$(EMACS) --batch -f batch-byte-compile 2>&1 $<

_update:
	@cask update 2>&1

.PHONY: all
