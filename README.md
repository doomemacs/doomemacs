![Release tag](https://img.shields.io/github/tag/hlissner/.emacs.d.svg?label=release&style=flat-square)
[![Master Build Status](https://img.shields.io/travis/hlissner/.emacs.d/master.svg?label=master&style=flat-square)](https://travis-ci.org/hlissner/.emacs.d)
[![Develop Build Status](https://img.shields.io/travis/hlissner/.emacs.d/develop.svg?label=develop&style=flat-square)](https://travis-ci.org/hlissner/.emacs.d)
[![MIT](https://img.shields.io/badge/license-MIT-green.svg?style=flat-square)](./LICENSE)

[![Main screenshot](/../screenshots/main.png)](/../screenshots)

- - -

<p align="center">
  <a href="/../../wiki">Documentation</a> |
  <a href="/../screenshots">Screenshots</a> |
  <a href="/../../wiki/Troubleshooting">Troubleshooting</a> |
  <a href="/../../wiki/FAQ">FAQ</a> |
  <a href="/CHANGELOG.org">Changelog</a>
</p>

- - -

<a href="http://ultravioletbat.deviantart.com/art/Yay-Evil-111710573">
  <img src="/../screenshots/cacochan.png" align="right" />
</a>

It is a story as old as time. A stubborn, shell-dwelling, and melodramatic
vimmer -- envious of the features of modern text editors -- spirals into despair
before he finally succumbs to the [dark side][evil-mode]. This is his config.

DOOM's philosophy is simple: be **fast**, be **readable**, and be **pretty**. It
is tailored for neckbeards with a blue-belt or better in command-line-fu who
don't shy away from dabbling with Elisp.

Rip and tear. Until it compiles.

> **Important:** Doom only supports Emacs >= 25.1, and is tailored for Arch
> Linux 4.7+ and Mac OS 10.11+.

- - -

## Quick start

```bash
git clone https://github.com/hlissner/.emacs.d ~/.emacs.d
cd ~/.emacs.d
cp init.example.el init.el  # maybe edit init.el
make install
```

Visit the wiki for [a more detailed guide on installing, customizing and
grokking Doom][wiki].

## Feature highlights

+ A fast, organized and opinionated Emacs configuration with a command line
  interface.
+ A custom, declarative [package management system][doom-packages] that combines
  package.el, [use-package] and [quelpa]. This lets you install packages from
  sources other than ELPA, as well as manage packages from the command line.
+ A [popup management system][doom-popups] (powered by [shackle]) that minimizes
  the presence and footprint of temporary and/or disposable buffers.
+ A vim-like experience with [evil-mode], including ports for several vim
  plugins, <kbd>C-x</kbd> omnicompletion and a slew of [custom ex commands][doom-my-commands].
+ Integration with [editorconfig]. Let someone else argue about tabs and spaces
  (spaces, duh).
+ Code completion for a variety of languages, powered by [company-mode] (there
  may be other dependencies for certain languages).
+ Project-awareness powered by [projectile], with tools to navigate and manage
  projects and project files.
+ Fast project search (and replace) utilities, powered by [the_silver_searcher],
  [ripgrep] and [wgrep], with [ivy] (the default), [helm] and ido integration.
+ Isolated and persistent workspaces powered by [persp-mode]. This can
  substitute for vim tabs.
+ Inline/live code evaluation (using [quickrun]), including REPLs for a variety
  of languages.

## Troubleshooting

Found a problem? Here are some things to try:

+ Make sure all plugins are installed with `make install`.
+ A `void-function` or `void-variable` might signal an out-of-date autoloads
  file. Update it with `make autoloads`.
+ Diagnose common OS/environment issues that could interfere with Emacs with
  `make doctor`.
+ If you byte-compiled Doom, run `make clean` or `M-x doom/clean-compiled-files`
  and restart Emacs. Never debug byte-compiled code, it will interfere with your
  efforts in subtle (and not-so-subtle) ways.

If all else fails, [file a bug report][doom-new-issue].

## Contribute

Doom (and my Emacs work in general) is a labor of love and incurable madness,
done on my free time. It wasn't intended for public use, but I enjoy making Doom
a resource for others.

If you'd like to support my efforts, I welcome contributions of any kind:

+ I love pull requests and bug reports (read the [contribution
  guidelines][wiki-contribute] first though!), and elisp pointers are especially
  welcome. Seriously, don't hesitate to [tell me my Elisp-fu
  sucks][doom-new-issue]!
+ I'm happy to discuss Emacs workflow, ideas or tooling. If you think I, Doom or
  other Emacs users could benefit from them (or you just want to chat), drop me
  a line at henrik@lissner.net. I'm eager to learn.


[wiki]: /../../wiki
[wiki-contribute]: /../../wiki/Contribute
[wiki-conventions]: /../../wiki/Conventions
[wiki-modules]: /../../wiki/Modules
[wiki-customization]: /../../wiki/Customization

[doom-my-bindings]: modules/private/hlissner/+bindings.el
[doom-my-commands]: modules/private/hlissner/+commands.el
[doom-new-issue]: https://github.com/hlissner/.emacs.d/issues/new
[doom-packages]: core/autoload/packages.el
[doom-popups]: core/core-popups.el
[doom-theme]: https://github.com/hlissner/emacs-doom-theme

[company-mode]: https://github.com/company-mode/company-mode
[editorconfig]: http://editorconfig.org/
[evil-mode]: https://github.com/emacs-evil/evil
[git-gutter-fringe]: https://github.com/syohex/emacs-git-gutter-fringe
[helm]: https://github.com/emacs-helm/helm
[ivy]: https://github.com/abo-abo/swiper
[persp-mode]: https://github.com/Bad-ptr/persp-mode.el
[projectile]: https://github.com/bbatsov/projectile
[quelpa]: https://github.com/quelpa/quelpa
[quickrun]: https://github.com/syohex/emacs-quickrun
[ripgrep]: https://github.com/BurntSushi/ripgrep
[shackle]: https://github.com/wasamasa/shackle
[the_silver_searcher]: https://github.com/ggreer/the_silver_searcher
[use-package]: https://github.com/jwiegley/use-package
[vim]: https://github.com/hlissner/.vim
[wgrep]: https://github.com/mhayashi1120/Emacs-wgrep

