[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](./LICENSE)

[![Yay! Evil!](https://raw.githubusercontent.com/hlissner/.emacs.d/screenshots/cacochan.png)](http://ultravioletbat.deviantart.com/art/Yay-Evil-111710573)

This is an Emacs configuration for a stubborn, melodramatic and shell-dwelling
vimmer disappointed with the text-editor status quo.

Doom tries to look and act like modern editors (whatever that will mean to me on
any given day), emulates vim as best it can and strives to surpass it in any way
possible. All to fit my needs as a software developer, indie gamedev, designer,
scientist and doom enthusiast.

It was tailored for Emacs 25+ on OSX 10.11+. I use [vim] everywhere else.

![Main screenshot](https://raw.githubusercontent.com/hlissner/.emacs.d/screenshots/main.png?raw=true)

**NOTE:** you can [find the theme in a separate repo](https://github.com/hlissner/emacs-doom-theme).

## Installation

```bash
# Ensure homebrew is up to date
brew update && brew upgrade

# Install cask + emacs
brew install cask
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-imagemagick --with-modern-icon --HEAD

# Install this emacs.d
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
* **[core/core-modeline.el](core/core-modeline.el)**: my modeline config
* **[private/my-bindings.el](private/my-bindings.el)**: most of the custom keybinds
* **[private/my-commands.el](private/my-commands.el)**: available custom ex commands
* **[ext/*.sh](ext/)**: scripts for external dependency setup (e.g. irony-mode)

See screenshots in the [screenshots branch][sc].

### Highlights

* Tamed popup windows with **[shackle]**; see `def-popup!` and `shackle-rules`.
  e.g. help buffers will always pop up at the bottom of the frame, and are
  removed with ESC.
* Workspaces & session persistence with **[workgroups2]**
* Project and workspace-sensitive buffer navigation and functions
* A vim-like environment with **[evil-mode]**
  * vim-seek/vim-sneak functionality with **[evil-snipe]** (provides 2-char motions)
  * [Multiple cursors][sc-multiedit] with **[evil-multiedit]**
  * Repeat (most) motions with <kbd>SPC</kbd> and
    <kbd>shift</kbd>+<kbd>SPC</kbd>
  * On-the-fly keybindings with `:[nviom]map`
  * Global <kbd>C-x</kbd> omnicompletion (e.g. <kbd>C-x</kbd>+<kbd>C-f</kbd> for
    files)
* Fast search utilities:
  * Project and buffer navigation with **[ivy]**
  * File browser sidebar with **[neotree]**
  * Project search (and replace) with **[counsel-ag]** and **[wgrep]**
  * Buffer search with **[swiper]**
* REPLs for many languages including Ruby, Python, PHP, JS, Elisp, Haskell and
  Lua.
* [Minimalistic diffs in the fringe][sc-diffs] with **[git-gutter-fringe]**.
* Modded **org-mode** to be a modern note-taking/LaTeX/word-processing platform. (WIP)
* Code debugging interface with **[realgud]** (currently supports gdb, trepanjs,
  bashdb and zshdb, working on Python/Ruby support)
* A do-what-I-mean jump-to-definition implementation that either uses major-mode
  commands or falls back to **[dumb-jump]**/ctags.
* A [pretty mode-line](core/core-modeline.el) with:
  * evil-search/iedit/evil-substitute mode-line integration
  * Macro-recording indicator
  * Python/ruby version in mode-line (for rbenv/pyenv)


### Other features

* Line numbers + highlight with **[nlinum]**
* On-demand [platform agnostic] shell with **eshell**
* Consistent marker-based code-folding with **hideshow**
* Inline code execution anywhere (once or live) with **[quickrun]**
* Snippet expansion and file templates with **[yasnippet]**
* Completion with **[company-mode]**
* Syntax checking with **[flycheck]**
* Custom O/S interaction commands, like **os-reveal** and **os-open-in-browser**
* Custom TODO, FIXME and NOTE highlighting and search (`:todo`)
* **big-mode** for presentations and demonstrations (`:big`)
* Tmux integration with `:t` and `:tcd` ex commands


## Troubleshooting

Though this wasn't designed with anyone else's use in mind, I'd be
happy to help anyone out with problems encountered using (or cribbing
from) my config. [Don't hesitate to report bugs](https://github.com/hlissner/.emacs.d/issues/new)!

A few things to keep in mind:

1. **Cask can be flakey, especially with new builds.** If you're
   getting odd errors when starting up Emacs, try to run `make
   install` again. I've also had cryptic cask errors that I had to
   reboot to resolve. YMMV.
2. If you add new functions to any of the autoloaded
   `(core|modules)/defuns/*.el` library files, run `make autoloads`
   afterwards. `:reload` will reload Emacs' load-path if you have
   Emacs open while doing so.


[auto-yasnippet]: https://melpa.org/#/auto-yasnippet
[company-mode]: https://melpa.org/#/company
[counsel-ag]: https://melpa.org/#/counsel
[dumb-jump]: https://melpa.org/#/dumb-jump
[evil-mode]: https://melpa.org/#/evil
[evil-multiedit]: https://melpa.org/#/evil-multiedit
[evil-snipe]: https://melpa.org/#/evil-snipe
[flycheck]: https://melpa.org/#/flycheck
[git-gutter-fringe]: https://melpa.org/#/git-gutter-fringe
[ivy]: https://melpa.org/#/ivy
[neotree]: https://melpa.org/#/neotree
[nlinum]: http://elpa.gnu.org/packages/nlinum.html
[quickrun]: https://melpa.org/#/quickrun
[realgud]: https://melpa.org/#/realgud
[repl-toggle]: https://melpa.org/#/repl-toggle
[shackle]: https://melpa.org/#/shackle
[swiper]: https://melpa.org/#/swiper
[workgroups2]: https://melpa.org/#/workgroups2
[wgrep]: https://melpa.org/#/wgrep
[yasnippet]: https://melpa.org/#/yasnippet

[sc]: https://github.com/hlissner/.emacs.d/tree/screenshots
[sc-diffs]: https://github.com/hlissner/.emacs.d/blob/screenshots/git-gutter.png?raw=true
[sc-multiedit]: https://raw.githubusercontent.com/hlissner/evil-multiedit/screenshots/main.gif?raw=true
[vim]: https://github.com/hlissner/.vim

