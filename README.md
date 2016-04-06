# NARF Emacs
![Screenshot](/../screenshots/main.png?raw=true)

> What we do every night, Pinky...

This is an Emacs configuration for stubborn vimmers and megalomaniacal mice alike. It
strives to emulate vim as best it can, and surpass it in any way possible.

## Installation

```
brew install cask
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-imagemagick --with-modern-icon
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
* **[private/my-bindings.el]**: most of the custom keybinds
* **[private/my-commands.el]**: available custom ex commands
* **[ext/Makefile](ext/Makefile)**: lists external dependencies

### Highlights

* Pretty line numbers with **nlinum** and neckbearded hackery
* Syntax checking with **Flycheck**
* Completion with **company-mode**
* Pretty mode-line with **spaceline**
* Project navigation with **helm** and **neotree**
* Project search with **ag** and file search with **evil-search** or **helm-swoop**
* Project-wide search and replace with **helm-ag** (press shift-Tab while in results
  buffer)
* Session persistence (and tab emulation) with **workgroups2**
  ![Workgroups2 tabs emulation](/../screenshots/tabs.png?raw=true)
* Run code inline with **quickrun**
* REPLs for many major modes with **repl-toggle**, including Ruby, Python,
  PHP, JS, Elisp, Haskell and Lua.
* 2-char motions with **evil-snipe**
* Diffs in the margin with **git-gutter**
* Repeat (most) motions with <kbd>SPC</kbd>
* Snippet expansion with **yasnippet** and **auto-yasnippet**
* File template support with **auto-insert** and **yasnippet**
* Code folding with **hideshow**
* Multiple cursors with **[evil-multiedit]**
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

    ![matches count in mode-line](/../screenshots/search.png?raw=true)
    ![substitutions count in mode-line](/../screenshots/subst.png?raw=true)
  * Macro-recording indicator:

    ![macro indicator in modeline](/../screenshots/macro.png?raw=true)
  * Show (py|rb)env version in mode-line

    ![py/rb version in modeline](/../screenshots/version.png?raw=true)

## What about Windo-
![Windows, you say...](http://i3.kym-cdn.com/photos/images/newsfeed/000/549/293/504.gif)


 [private/my-bindings.el]: private/my-bindings.el
 [private/my-commands.el]: private/my-commands.el
 [evil-mc]: https://github.com/gabesoft/evil-mc
 [multiple-cursors]: https://github.com/magnars/multiple-cursors.el
 [evil-multiedit]: https://github.com/hlissner/evil-multiedit
