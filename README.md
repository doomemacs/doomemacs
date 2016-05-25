[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](./LICENSE)

This is an Emacs configuration for stubborn vimmers and silent demon
annihilating protagonists alike. It strives to emulate vim as best it can, and
surpass it in any way possible.

![Splash page screenshot](https://raw.githubusercontent.com/hlissner/.emacs.d/screenshots/scratch.png?raw=true)
![Main screenshots](https://raw.githubusercontent.com/hlissner/.emacs.d/screenshots/main.png?raw=true)

## Installation

```
brew install cask
brew install emacs --with-cocoa --with-imagemagick
git clone https://github.com/hlissner/.emacs.d ~/.emacs.d
cd ~/.emacs.d
make          # installs plugins via cask and generates autoloads
```

And to optionally:

```
make compile  # optionally byte-compiles everything
make snippets # install hlissner/emacs-snippets into private/snippets
```

## Features

To get a picture of what's in here, check out:

* **[The Caskfile](Cask)**: lists installed plugins and where they're configured.
* **[init.el](init.el)**: lists all loaded modules
* **[private/my-bindings.el](private/my-bindings.el)**: most of the custom keybinds
* **[private/my-commands.el](private/my-commands.el)**: available custom ex commands
* **[ext/Makefile](ext/Makefile)**: lists external dependencies

### Highlights

* Pretty line numbers with **nlinum** and neckbearded hackery
* Syntax checking with **Flycheck**
* Completion with **company-mode**
* Nigh-universal code debugging interface with **realgud**
* Pretty mode-line with **spaceline**
* Project navigation with **helm** and **neotree**
* Project search with **ag** and file search with **evil-search** or **helm-swoop**
* Project-wide search and replace with **helm-ag** (press shift-Tab while in results
  buffer)
* Session persistence (and tab emulation) with **workgroups2**
  ![Workgroups2 tabs emulation](https://raw.githubusercontent.com/hlissner/.emacs.d/screenshots/tabs.png?raw=true)
* Run code inline with **quickrun**
* REPLs for many major modes with **repl-toggle**, including Ruby, Python,
  PHP, JS, Elisp, Haskell and Lua.
* 2-char motions with **evil-snipe**
* Diffs in the margin with **git-gutter**
* Repeat (most) motions with <kbd>SPC</kbd>
* Snippet expansion with **yasnippet** and **auto-yasnippet**
* File template support with **auto-insert** and **yasnippet**
* Code folding with **hideshow**
* Multiple cursors with **[evil-multiedit](https://github.com/hlissner/evil-multiedit)**
* O/S interaction functions, like **os-reveal** and **os-open-in-browser**
* Custom TODO, FIXME and NOTE highlighting
* **big-mode** for presentations and demonstrations
* Tmux integration with `:t` and `:tcd` ex commands
* Tamed popup windows with **shackle**
* Vim-esque omnicompletion. e.g. `C-x C-f` for files
* Quick keybindings with `:[nviom]map`
* Emacs for modern note-taking/LaTeX/writing with **org-mode** or **rst-mode**
* **Modeline improvements**
  * evil-search/iedit/evil-substitute mode-line integration:

    ![matches count in mode-line](https://raw.githubusercontent.com/hlissner/.emacs.d/screenshots/search.png?raw=true)
    ![substitutions count in mode-line](https://raw.githubusercontent.com/hlissner/.emacs.d/screenshots/subst.png?raw=true)
  * Macro-recording indicator:

    ![macro indicator in modeline](https://raw.githubusercontent.com/hlissner/.emacs.d/screenshots/macro.png?raw=true)
  * Show (py|rb)env version in mode-line

    ![py/rb version in modeline](https://raw.githubusercontent.com/hlissner/.emacs.d/screenshots/version.png?raw=true)

---
![Yay! Evil!](https://raw.githubusercontent.com/hlissner/.emacs.d/screenshots/cacochan.png)
