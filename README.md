![Release tag](https://img.shields.io/github/tag/hlissner/.emacs.d.svg?label=release&style=flat-square)
[![Master Build Status](https://img.shields.io/travis/hlissner/.emacs.d/master.svg?label=master&style=flat-square)](https://travis-ci.org/hlissner/.emacs.d)
[![Develop Build Status](https://img.shields.io/travis/hlissner/.emacs.d/develop.svg?label=develop&style=flat-square)](https://travis-ci.org/hlissner/.emacs.d)
[![MIT](https://img.shields.io/badge/license-MIT-green.svg?style=flat-square)](./LICENSE)

[![Main screenshot](/../screenshots/main.png?raw=true)][sc]

<a href="http://ultravioletbat.deviantart.com/art/Yay-Evil-111710573">
  <img src="https://raw.githubusercontent.com/hlissner/.emacs.d/screenshots/cacochan.png" align="right" />
</a>

This is an Emacs configuration for a stubborn, shell-dwelling and melodramatic
vimmer disappointed with the text-editor status quo.

Doom tries to: look and act like modern editors (whatever that means to me on
any given day), espouse vim's modal philosophy as best it can and strive to
surpass vim in any way possible. It fits my needs as a software developer, indie
game developer, scientist and doom enthusiast.

It was written for **Emacs 25.1+** on **MacOS 10.11+** and **Arch Linux 4.7+**.
I use [vim] everywhere else.

## Installation

```bash
git clone https://github.com/hlissner/.emacs.d ~/.emacs.d
cd ~/.emacs.d
cp init.example.el init.el  # maybe edit init.el
make install

# Have problems? Run this to check for common issues with your setup
make doctor
```

Once you've tweaked the config to your liking, you may optionally byte-compile
it. DOOM is designed to benefit from this. It will boost startup times and make
Emacs feel a bit snappier in general.

```bash
make compile  # may take a while
# or
make core     # faster alternative; only compiles init.el & core files

# If you byte-compile, changes to the config won't take effect until you
# recompile or delete the byte-compiled files with:
make clean
```

## Package Management

Plugins can be managed from the command line with `make`:

```bash
make install     # install missing plugins
make update      # update installed plugins
make autoremove  # remove unused plugins
# be sure to run install and autoremove after modifying init.el

# run this if you change autoload files
make autoloads

# this is the equivalent of running all four of the above commands
make

# you can run any make command with DEBUG=1 for extra logging, and YES=1 to
# auto-accept confirmation prompts:
DEBUG=1 make install
YES=1 make update
```

These commands are also available from within Emacs:

+ `doom/packages-install`
+ `doom/packages-update`
+ `doom/packages-autoremove`
+ `doom/reload-autoloads`

## Deciphering my emacs.d

So you want to grok this madness. Here are a few suggestions:

* **[init.example.el](init.example.el)**: a birds eye view of available modules
* **[modules/README.org](modules/README.org)**: a primer into module structure
* **[modules/private/hlissner/+bindings.el](modules/private/hlissner/+bindings.el)**:
  my custom keybinds.
* **[modules/private/hlissner/+commands.el](modules/private/hlissner/+commands.el)**:
  my custom ex-commands (for [evil-mode]).
* **[modules/ui](modules/ui)**: the modules that makes my Emacs look the way it
  does, including [my theme][doom-theme], modeline, dashboard and more.
* Find screenshots in the [screenshots branch][sc].

### Highlights

* A [popup management system](core/core-popups.el) using **[shackle]** to
  minimize mental context switching while dealing with temporary or disposable
  buffers.
* Per-project code-style settings with **[editorconfig]**. Let someone else
  argue about tabs versus spaces (spaces, of course).
* Workspaces & session persistence with **[persp-mode]**. Provides tab emulation
  that vaguely resembles vim's tabs.
* Project & workspace-restricted buffer navigation and functions.
* A vim-centric environment with **[evil-mode]**
  * 2-character motions (ala vim-seek/vim-sneak) with **[evil-snipe]**
  * Sublime Text-esque [multiple cursors][sc-multiedit] with
    **[evil-mc]** and **[evil-multiedit]**
  * <kbd>C-x</kbd> omnicompletion in insert mode
  * A better `:global` with buffer highlighting
  * A slew of [custom ex commands](modules/private/hlissner/+commands.el)
* Fast search utilities:
  * Project and buffer navigation with **[ivy]**
  * File browser sidebar with **[neotree]**
  * Project text search powered by [the silver searcher][ag] and [ripgrep][rg]
    (see `:ag` and `:rg`)
  * Project search & replace with **[wgrep]**
  * Interactive buffer search with **[swiper]**
* Inline/live code evaluation (using **[quickrun]**) and REPLs for a variety of
  languages, including Ruby, Python, PHP, JS, Elisp, Haskell, Lua and more.
* [Minimalistic diffs in the fringe][sc-diffs] with **[git-gutter-fringe]**.
* A do-what-I-mean jump-to-definition implementation that tries its darnest to
  find the definition of what you're looking at. It tries major-mode commands,
  xref (experimental Emacs library), **[dumb-jump]**, ctags (WIP), then
  **[ripgrep][rg]** or **[the_silver_searcher][ag]**.
* Snippets and file-templates with **[yasnippet]** & **[auto-yasnippet]**.
* A smarter, perdier, Atom-inspired mode-line that adds:
  * evil-search/iedit/evil-substitute mode-line integration
  * Macro-recording indicator
  * Python/ruby version in mode-line (for rbenv/pyenv)
* Emacs as an:
  * Email client (using mu4e & offlineimap)
  * Presentation app (using org-tree-slides, ox-reveal, +present/big-mode
    & impatient-mode)
  * RSS feed reader (using elfeed)
  * Word Processor (using LaTeX, Org and Markdown)

## Troubleshooting

My config wasn't intended for public use, but I'm happy to help you use or crib
from it.

+ If you have questions, drop me a line at henrik@lissner.net.
+ If you have issues running or setting up DOOM, use `make doctor` to diagnose
  any common problems.
+ If you still can't make sense of it, run `DEBUG=1 make doctor` and include
  it [with your bug report][new-issue].

**And please include steps to reproduce your issue, if possible.**

## Contributing

I welcome contributions of any kind: documentation, bug fixes/reports, extra
modules, even elisp tips. Really,
[don't hesitate to tell me my Elisp-fu sucks][new-issue]! I'm eager to learn.


[ag]: https://github.com/ggreer/the_silver_searcher
[auto-yasnippet]: https://melpa.org/#/auto-yasnippet
[company-mode]: https://melpa.org/#/company
[doom-theme]: https://github.com/hlissner/emacs-doom-theme
[dumb-jump]: https://melpa.org/#/dumb-jump
[editorconfig]: http://editorconfig.org/
[evil-mc]: https://github.com/gabesoft/evil-mc
[evil-mode]: https://melpa.org/#/evil
[evil-multiedit]: https://melpa.org/#/evil-multiedit
[evil-snipe]: https://melpa.org/#/evil-snipe
[git-gutter-fringe]: https://melpa.org/#/git-gutter-fringe
[ivy]: https://melpa.org/#/ivy
[neotree]: https://melpa.org/#/neotree
[new-issue]: https://github.com/hlissner/.emacs.d/issues/new
[persp-mode]: https://melpa.org/#/persp-mode
[quickrun]: https://melpa.org/#/quickrun
[rg]: https://github.com/BurntSushi/ripgrep
[sc-diffs]: https://github.com/hlissner/.emacs.d/blob/screenshots/git-gutter.png?raw=true
[sc-multiedit]: https://raw.githubusercontent.com/hlissner/evil-multiedit/screenshots/main.gif?raw=true
[sc]: https://github.com/hlissner/.emacs.d/tree/screenshots
[shackle]: https://melpa.org/#/shackle
[swiper]: https://melpa.org/#/swiper
[vim]: https://github.com/hlissner/.vim
[wgrep]: https://melpa.org/#/wgrep
[yasnippet]: https://melpa.org/#/yasnippet
[yay-evil]: http://ultravioletbat.deviantart.com/art/Yay-Evil-111710573
