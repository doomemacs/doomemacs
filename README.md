[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](./LICENSE)

This is an Emacs configuration for stubborn vimmers and silent demon
annihilating protagonists alike. It strives to emulate vim as best it can, and
surpass it in any way possible.

<center>
[![Yay! Evil!](https://raw.githubusercontent.com/hlissner/.emacs.d/screenshots/cacochan.png)](http://ultravioletbat.deviantart.com/art/Yay-Evil-111710573)
</center>
<br />

It is tailored to OSX 10.11+, Emacs 25+ and my needs as a software developer,
designer, scientist and doom enthusiast.

![Splash page screenshot](https://raw.githubusercontent.com/hlissner/.emacs.d/screenshots/scratch.png?raw=true)
![Main screenshots](https://raw.githubusercontent.com/hlissner/.emacs.d/screenshots/main.png?raw=true)

## Installation

```
brew install cask
brew install emacs --with-cocoa --with-imagemagick
git clone https://github.com/hlissner/.emacs.d ~/.emacs.d
cd ~/.emacs.d
make          # installs plugins via cask and generates autoloads

# Optional
make compile  # compiles core and autoloaded files
make snippets # install hlissner/emacs-snippets into private/snippets
```

For a more comprehensive byte compile, run `:bc!` (`:bc` will compile the
current *.el file).

## Features

To get a picture of what's in here, check out:

* **[The Caskfile](Cask)**: lists installed plugins and where they're configured.
* **[init.el](init.el)**: lists all loaded modules
* **[private/my-bindings.el](private/my-bindings.el)**: most of the custom keybinds
* **[private/my-commands.el](private/my-commands.el)**: available custom ex commands
* **[ext/Makefile](ext/Makefile)**: lists external dependencies

### Highlights

* Line numbers + highlight with **[nlinum]** <br />
* Syntax checking with **[flycheck]** <br />
* Completion with **[company-mode]** <br />
* Nigh-universal code debugging interface with **[realgud]**
* Project navigation with **[ivy]** and **[neotree]**
* Project search with **[counsel-ag]**
* Buffer search with **evil-search** and **[swiper]**
* Session persistence (and tab emulation) with **[workgroups2]**
* Run code inline with **[quickrun]**
* REPLs for many major modes with **[repl-toggle]**, including Ruby, Python, PHP,
  JS, Elisp, Haskell and Lua.
* Minimalistic diffs in the margin with **[git-gutter-fringe]**
* Snippet expansion with **[yasnippet]** and **[auto-yasnippet]**
* File template support with **auto-insert** and **[yasnippet]**
* Code folding with **hideshow**
* Custom O/S interaction commands, like **os-reveal** and **os-open-in-browser**
* Custom TODO, FIXME and NOTE highlighting
* **big-mode** for presentations and demonstrations
* Tmux integration with `:t` and `:tcd` ex commands
* Tamed popup windows with **[shackle]**
* emacs for modern note-taking/LaTeX/writing with **org-mode** or **rst-mode**
* Vim-esque Emacs with **[evil-mode]**, plus:
  * 2-char motions with **[evil-snipe]**
  * Repeat (most) motions with <kbd>SPC</kbd>
  * Multiple cursors with **[evil-multiedit]**
  * Quick keybindings with `:[nviom]map`
  * Vim-esque omnicompletion. e.g. `C-x C-f` for files
* Pretty mode-line with **[spaceline]**, plus:
  * evil-search/iedit/evil-substitute mode-line integration
  * Indicator when macro is recording<br/>
  * Show python/ruby version in mode-line (with rbenv/pyenv)


[nlinum]: http://elpa.gnu.org/packages/nlinum.html
[flycheck]: https://melpa.org/#/flycheck
[company-mode]: https://melpa.org/#/company
[realgud]: https://melpa.org/#/realgud
[ivy]: https://melpa.org/#/ivy
[git-gutter-fringe]: https://melpa.org/#/git-gutter-fringe
[neotree]: https://melpa.org/#/neotree
[counsel-ag]: https://melpa.org/#/counsel
[swiper]: https://melpa.org/#/swiper
[evil-mode]: https://melpa.org/#/evil
[workgroups2]: https://melpa.org/#/workgroups2
[quickrun]: https://melpa.org/#/quickrun
[repl-toggle]: https://melpa.org/#/repl-toggle
[yasnippet]: https://melpa.org/#/yasnippet
[auto-yasnippet]: https://melpa.org/#/auto-yasnippet
[shackle]: https://melpa.org/#/shackle
[evil-snipe]: https://melpa.org/#/evil-snipe
[evil-multiedit]: https://melpa.org/#/evil-multiedit
[spaceline]: https://melpa.org/#/spaceline

## More screenshots

In the [screenshots branch](https://github.com/hlissner/.emacs.d/tree/screenshots).
