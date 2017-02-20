[![Main screenshot](https://raw.githubusercontent.com/hlissner/.emacs.d/screenshots/main.png?raw=true)][sc]

[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](./LICENSE)
[![DOOM v2.0.0](https://img.shields.io/badge/DOOM-v2.0.0-blue.svg)](./init.el)

<a href="http://ultravioletbat.deviantart.com/art/Yay-Evil-111710573">
  <img src="https://raw.githubusercontent.com/hlissner/.emacs.d/screenshots/cacochan.png" align="right" />
</a>

This is an Emacs configuration for a stubborn, melodramatic and
shell-dwelling vimmer disappointed with the text-editor status quo.

Doom tries to look and act like modern editors (whatever that means to
me on any given day), emulate vim as best it can and strive to surpass
it in any way possible. I've designed it to fit my needs as a software
developer, indie game developer, scientist and doom enthusiast.

It was tailored for **MacOS 10.11+** and **Arch Linux 4.7+**, and
**exclusively** for Emacs 25.1+. I use [vim] everywhere else.

## Installation

```bash
git clone https://github.com/hlissner/.emacs.d ~/.emacs.d
cd ~/.emacs.d
cp init.example.el init.el  # maybe edit init.el
make install
make compile       # optional, may take a while
```

Run `make` each time you change init.el (and recompile, if necessary).
Running `make` is the equivalent of doing:

```bash
make install       # or (doom/packages-install)
make autoloads     # or (doom/reload-autoloads)
```

## Deciphering my emacs.d

To get a picture of what's in here, check out:

* **[init.example.el](init.example.el)**: what my loadout looks like.
* **[modules/README.md](modules/README.md)**: a primer into module
  structure and how the module system works.
* **[modules/private/hlissner/+bindings.el](modules/private/hlissner/+bindings.el)**:
  my custom keybinds.
* **[modules/private/hlissner/+commands.el](modules/private/hlissner/+commands.el)**:
  my custom ex commands.
* **[modules/ui](modules/ui)**: the modules that makes my Emacs look
  the way it does, including [my theme][doom-theme], modeline,
  dashboard and more.
+ Find screenshots in the [screenshots branch][sc].

### Highlights

* A [popup management system](core/core-popups.el) using **[shackle]**
  for temporary or disposable buffers.
* Workspaces, tab emulation & session persistence with
  **[persp-mode]**.
* Project and workspace-restricted buffer navigation and functions.
* A vim-centric environment with **[evil-mode]**
  * 2-character motions (ala vim-seek/vim-sneak) with **[evil-snipe]**
  * Sublime Text-esque [multiple cursors][sc-multiedit] with
    **[evil-multiedit]**
  * Repeat (most) motions with <kbd>SPC</kbd> and
    <kbd>shift</kbd>+<kbd>SPC</kbd> (backwards)
  * Global <kbd>C-x</kbd> omnicompletion
* Fast search utilities:
  * Project and buffer navigation with **[ivy]**
  * File browser sidebar with **[neotree]**
  * Project search (& replace) with **[counsel-ag]** (and **[wgrep]**)
  * Interactive buffer search with **[swiper]**
* REPLs & inline/live code evaluation (using **[quickrun]** and
  **[repl-toggle]**) for many languages including Ruby, Python, PHP,
  JS, Elisp, Haskell and Lua.
* [Minimalistic diffs in the fringe][sc-diffs] with **[git-gutter-fringe]**.
* A do-what-I-mean jump-to-definition implementation that tries its
  darnest to find the definition of what you're looking at. It tries
  major-mode commands, **[dumb-jump]**, ctags, then **[counsel-ag]**.
* Snippets and file-templates with **[yasnippet]**.
* A smarter, perdier, Atom-inspired mode-line that includes:
  * evil-search/iedit/evil-substitute mode-line integration
  * Macro-recording indicator
  * Python/ruby version in mode-line (for rbenv/pyenv)

## Contributing or troubleshooting

My config wasn't intended for public use, but I'm happy to help you
use or crib from my config and I welcome contributions of any kind;
documentation, bug fixes or even elisp tips.

[Don't hesitate to report bugs or tell me my Elisp-fu sucks](https://github.com/hlissner/.emacs.d/issues/new)!


[yay-evil]: http://ultravioletbat.deviantart.com/art/Yay-Evil-111710573
[Cask]: https://github.com/cask/cask

[company-mode]: https://melpa.org/#/company
[counsel-ag]: https://melpa.org/#/counsel
[dumb-jump]: https://melpa.org/#/dumb-jump
[evil-mode]: https://melpa.org/#/evil
[evil-multiedit]: https://melpa.org/#/evil-multiedit
[evil-snipe]: https://melpa.org/#/evil-snipe
[git-gutter-fringe]: https://melpa.org/#/git-gutter-fringe
[ivy]: https://melpa.org/#/ivy
[neotree]: https://melpa.org/#/neotree
[quickrun]: https://melpa.org/#/quickrun
[repl-toggle]: https://melpa.org/#/repl-toggle
[shackle]: https://melpa.org/#/shackle
[swiper]: https://melpa.org/#/swiper
[wgrep]: https://melpa.org/#/wgrep
[persp-mode]: https://melpa.org/#/persp-mode
[yasnippet]: https://melpa.org/#/yasnippet

[sc]: https://github.com/hlissner/.emacs.d/tree/screenshots
[sc-diffs]: https://github.com/hlissner/.emacs.d/blob/screenshots/git-gutter.png?raw=true
[sc-multiedit]: https://raw.githubusercontent.com/hlissner/evil-multiedit/screenshots/main.gif?raw=true
[vim]: https://github.com/hlissner/.vim
[doom-theme]: https://github.com/hlissner/emacs-doom-theme
