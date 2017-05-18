[![Main screenshot](/../screenshots/main.png?raw=true)][sc]

[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](./LICENSE)
[![DOOM v2.0.2](https://img.shields.io/badge/DOOM-v2.0.2-blue.svg)](./init.el)
[![Build Status](https://travis-ci.org/hlissner/.emacs.d.png?branch=v2)](https://travis-ci.org/hlissner/.emacs.d)

<a href="http://ultravioletbat.deviantart.com/art/Yay-Evil-111710573">
  <img src="https://raw.githubusercontent.com/hlissner/.emacs.d/screenshots/cacochan.png" align="right" />
</a>

This is an Emacs configuration for a stubborn, melodramatic and shell-dwelling
vimmer disappointed with the text-editor status quo.

Doom tries to: look and act like modern editors (whatever that means to me on
any given day), espouse vim's modal philosophy as best it can and strive to
surpass vim in any way possible. It fits my needs as a software developer, indie
game developer, scientist and doom enthusiast.

It was tailored for **Emacs 25.1+** on **MacOS 10.11+** and **Arch Linux 4.7+**.
I use [vim] everywhere else.

## Installation

```bash
git clone https://github.com/hlissner/.emacs.d ~/.emacs.d
cd ~/.emacs.d
cp init.example.el init.el  # maybe edit init.el
make install
make compile       # optional, may take a while
make compile-lite  # optional (lighter alternative to compile)
```

Run `make` after making changes to modules (like adding packages or autoloaded
functions). This is the equivalent of:

```bash
make install       # or (doom/packages-install)
make autoloads     # or (doom/reload-autoloads)
```

You can run any Make command with `DEBUG=1` for added logging verbosity, and
`YES=1` to auto-accept any confirmation prompts.

## Deciphering my emacs.d

So you want to grok some of this madness. Here are a few suggestions:

* **[init.example.el](init.example.el)**: a birds eye view of available modules
* **[modules/README.md](modules/README.md)**: a primer into module structure and
  how the module system works.
* **[modules/private/hlissner/+bindings.el](modules/private/hlissner/+bindings.el)**:
  my custom keybinds.
* **[modules/private/hlissner/+commands.el](modules/private/hlissner/+commands.el)**:
  my custom ex commands.
* **[modules/ui](modules/ui)**: the modules that makes my Emacs look the way it
  does, including [my theme][doom-theme], modeline, dashboard and more.
* Find screenshots in the [screenshots branch][sc].

### Highlights

* A [popup window management system](core/core-popups.el) using **[shackle]** to
  minimize mental context switching while dealing with temporary or disposable
  buffers.
* Per-project code-style settings with **[editorconfig]**. Let someone else
  argue about tabs versus spaces (spaces > tabs, btw).
* Workspaces & session persistence with **[persp-mode]**. This provides tab
  emulation that vaguely resembles vim tabs.
* Project & workspace-aware buffer navigation and functions.
* A vim-centric environment with **[evil-mode]**
  * 2-character motions (ala vim-seek/vim-sneak) with **[evil-snipe]**
  * Sublime Text-esque [multiple cursors][sc-multiedit] with
    **[evil-mc]** and **[evil-multiedit]**
  * Repeat (most) motions with <kbd>SPC</kbd> and
    <kbd>shift</kbd>+<kbd>SPC</kbd> (backwards)
  * <kbd>C-x</kbd> omnicompletion in insert mode
* Fast search utilities:
  * Project and buffer navigation with **[ivy]**
  * File browser sidebar with **[neotree]**
  * Project text search powered by [the silver searcher][ag] and [ripgrep][rg]
    (see `:ag` and `:rg`)
  * Project search & replace with **[wgrep]**
  * Interactive buffer search with **[swiper]**
* REPLs & inline/live code evaluation (using **[quickrun]**) with languages
  support for Ruby, Python, PHP, JS, Elisp, Haskell, Lua and more.
* [Minimalistic diffs in the fringe][sc-diffs] with **[git-gutter-fringe]**.
* A do-what-I-mean jump-to-definition implementation that tries its darnest to
  find the definition of what you're looking at. It tries major-mode commands,
  xref (experimental Emacs library) **[dumb-jump]**, ctags (WIP), then
  **[counsel-ag]**.
* Snippets and file-templates with **[yasnippet]**.
* A smarter, perdier, Atom-inspired mode-line that includes:
  * evil-search/iedit/evil-substitute mode-line integration
  * Macro-recording indicator
  * Python/ruby version in mode-line (for rbenv/pyenv)
* Emacs as an:
  * Email client (using mu4e & offlineimap)
  * Presentation app (using org-tree-slides, ox-reveal, +present/big-mode
    & impatient-mode)
  * RSS feed reader (using elfeed)
  * Word Processor (using LaTeX, Org and Markdown)

## Contributing or troubleshooting

My config wasn't intended for public use, but I'm happy to help you use or crib
from my config. I welcome contributions of any kind; documentation, bug
fixes/reports, even elisp tips.

[Don't hesitate to tell me my Elisp-fu sucks](https://github.com/hlissner/.emacs.d/issues/new)!


[ag]: https://github.com/ggreer/the_silver_searcher
[company-mode]: https://melpa.org/#/company
[counsel-ag]: https://melpa.org/#/counsel
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
[persp-mode]: https://melpa.org/#/persp-mode
[quickrun]: https://melpa.org/#/quickrun
[ripgrep]: https://github.com/BurntSushi/ripgrep
[sc-diffs]: https://github.com/hlissner/.emacs.d/blob/screenshots/git-gutter.png?raw=true
[sc-multiedit]: https://raw.githubusercontent.com/hlissner/evil-multiedit/screenshots/main.gif?raw=true
[sc]: https://github.com/hlissner/.emacs.d/tree/screenshots
[shackle]: https://melpa.org/#/shackle
[swiper]: https://melpa.org/#/swiper
[vim]: https://github.com/hlissner/.vim
[wgrep]: https://melpa.org/#/wgrep
[yasnippet]: https://melpa.org/#/yasnippet
[yay-evil]: http://ultravioletbat.deviantart.com/art/Yay-Evil-111710573
