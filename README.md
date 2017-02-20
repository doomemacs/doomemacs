[![Main screenshot](https://raw.githubusercontent.com/hlissner/.emacs.d/screenshots/main.png?raw=true)][sc]

[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](./LICENSE)
[![DOOM v2.0.0](https://img.shields.io/badge/DOOM-v2.0.0-blue.svg)](./init.el)

<a href="http://ultravioletbat.deviantart.com/art/Yay-Evil-111710573">
  <img src="https://raw.githubusercontent.com/hlissner/.emacs.d/screenshots/cacochan.png" align="right" />
</a>

This is an Emacs configuration for a stubborn, melodramatic and
shell-dwelling vimmer disappointed with the text-editor statsu quo.

Doom tries to look and act like modern editors (whatever that means to
me on any given day), emulate vim as best it can and strive to surpass
it in any way possible. I've designed it to fit my needs as a software
developer, indie game developer, scientist and doom enthusiast.

It was tailored for **MacOS 10.11+** and **Arch Linux 4.7+**, and
exclusively for Emacs 25.1+ **(NOTE: Older versions of Emacs won't
work).**. I use [vim] everywhere else.

**NOTE:** you can [find the theme in a separate repo][doom-theme].

## Installation

```bash
git clone https://github.com/hlissner/.emacs.d ~/.emacs.d
cd ~/.emacs.d
cp init.example.el init.el  # maybe edit init.el
make install
make compile       # optional, may take a while
```

If you change the module list in `init.el`, you'll need to refresh
autoloads and, if necessary, recompile:

+ `make autoloads` or `(doom/reload-autoloads)`
+ If you byte-compiled: `make compile` or `(doom/byte-compile)`

## Deciphering my emacs.d

To get a picture of what's in here, check out:

* **[init.example.el](init.example.el)**: what my init.el looks like.
  Copy this to `init.el` and you're set.
* **[modules/README.md](modules/README.md)**: a primer into module
  structure and how the module system works.
* **[modules/private/hlissner/+bindings.el](modules/private/hlissner/+bindings.el)**:
  my custom keybinds.
* **[modules/private/hlissner/+commands.el](modules/private/hlissner/+commands.el)**:
  my custom ex commands.
* **[modules/ui](modules/ui)**: the modules that makes my Emacs look
  the way it does, including my modeline, dashboard and more.
+ See screenshots in the [screenshots branch][sc].
+ Some modules have README's.

### Features

* Strong support for a [large selection of languages](modules/lang),
  including REPLs and inline/live code evaluation.
* A [consistent popup management system](core/core-popups.el) using
  **[shackle]** for temporary or disposable buffers. e.g. help buffers
  will always pop up at the bottom of the frame, and are removed with
  ESC.
* Workspaces, tab emulation and session persistence with
  **[workgroups2]**.
* Project and workspace-sensitive buffer navigation and functions.
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
* REPLs & inline/live code evaluation (using **[quickrun]**) for many
  languages including Ruby, Python, PHP, JS, Elisp, Haskell and Lua.
* [Minimalistic diffs in the fringe][sc-diffs] with **[git-gutter-fringe]**.
* A do-what-I-mean jump-to-definition implementation that tries its
  darnest to find the definition of what you're looking at. It tries
  major-mode commands, **[dumb-jump]**, ctags, then **[counsel-ag]**.
* Snippets and file-templates with **[yasnippet]**.
* Hybrid completion with both **[company-mode]** and
  **[auto-complete]**. They collaborate to give you the completion
  system most appropriate for the current major mode.
* A modern interface inspired by Atom's, with a smarter, perdier
  mode-line that includes:
  * evil-search/iedit/evil-substitute mode-line integration
  * Macro-recording indicator
  * Python/ruby version in mode-line (for rbenv/pyenv)

## Contributing or troubleshooting

My config wasn't designed with anyone else's use in mind, but I'm all
for improving it in any way possible. [Don't hesitate to report bugs
or tell me my Elisp-fu sucks](https://github.com/hlissner/.emacs.d/issues/new)!

If you'd like to help, I'd be happy to accept any sort of
contributions, whether that be modules, extra documentation, bug fixes
or even elisp tips. I don't mind any opportunity to learn more about
Emacs.


[yay-evil]: http://ultravioletbat.deviantart.com/art/Yay-Evil-111710573
[Cask]: https://github.com/cask/cask
[Homebrew]: http://brew.sh

[auto-yasnippet]: https://melpa.org/#/auto-yasnippet
[company-mode]: https://melpa.org/#/company
[counsel-ag]: https://melpa.org/#/counsel
[dumb-jump]: https://melpa.org/#/dumb-jump
[evil-mode]: https://melpa.org/#/evil
[evil-multiedit]: https://melpa.org/#/evil-multiedit
[evil-snipe]: https://melpa.org/#/evil-snipe
[flycheck]: https://melpa.org/#/flycheck
[git-gutter-fringe]: https://melpa.org/#/git-gutter-fringe
[irony-mode]: https://github.com/Sarcasm/irony-mode
[ivy]: https://melpa.org/#/ivy
[neotree]: https://melpa.org/#/neotree
[nlinum]: http://elpa.gnu.org/packages/nlinum.html
[quickrun]: https://melpa.org/#/quickrun
[racer]: https://github.com/phildawes/racer
[realgud]: https://melpa.org/#/realgud
[repl-toggle]: https://melpa.org/#/repl-toggle
[shackle]: https://melpa.org/#/shackle
[swiper]: https://melpa.org/#/swiper
[wgrep]: https://melpa.org/#/wgrep
[workgroups2]: https://melpa.org/#/workgroups2
[yasnippet]: https://melpa.org/#/yasnippet

[sc]: https://github.com/hlissner/.emacs.d/tree/screenshots
[sc-diffs]: https://github.com/hlissner/.emacs.d/blob/screenshots/git-gutter.png?raw=true
[sc-multiedit]: https://raw.githubusercontent.com/hlissner/evil-multiedit/screenshots/main.gif?raw=true
[vim]: https://github.com/hlissner/.vim
[doom-theme]: https://github.com/hlissner/emacs-doom-theme
