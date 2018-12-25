<p align="center">
  <img src="https://img.shields.io/github/tag/hlissner/doom-emacs.svg?label=release&style=flat-square" alt="Current release" />
  <a href="https://travis-ci.org/hlissner/doom-emacs"><img src="https://img.shields.io/travis/hlissner/doom-emacs/master.svg?label=master&style=flat-square" alt="build status (master)" /></a>
  <a href="https://travis-ci.org/hlissner/doom-emacs"><img src="https://img.shields.io/travis/hlissner/doom-emacs/develop.svg?label=develop&style=flat-square" alt="build status (develop)" /></a>
  <a href="https://discord.gg/bcZ6P3y"><img src="https://img.shields.io/badge/discord-chat-blue.svg?style=flat-square&logo=discord&colorB=%237289DA" alt="Chat on discord" /></a>
  <img src="https://github.com/hlissner/doom-emacs/raw/screenshots/main.png" alt="Main screenshot" />
</p>

- - -

<p align="center">
  <a href="/../../wiki">Wiki</a> |
  <a href="/../../tree/screenshots">Screenshots</a> |
  <a href="/../../wiki/FAQ#troubleshooting">Troubleshooting</a> |
  <a href="/../../wiki/FAQ">FAQ</a> |
  <a href="/../develop/CHANGELOG.org">Changelog</a>
</p>

- - -

<a href="http://ultravioletbat.deviantart.com/art/Yay-Evil-111710573">
  <img src="/../screenshots/cacochan.png" align="right" />
</a>

It is a story as old as time. A stubborn, shell-dwelling, and melodramatic
vimmer -- envious of the features of modern text editors -- spirals into despair
before finally succumbing to the [dark side][evil-mode]. This is his config.

Doom strives to be fast, fabulous and hacker friendly. It is tailored for
neckbeards with blue belts or better in command-line-fu, Elisp and git.

> Doom **only** supports Emacs >= 25.1, and is tested on Arch Linux 4.7+ and
> MacOS 10.11. YMMV on other platforms.

- - -

## Quick start

```bash
git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
cd ~/.emacs.d
make quickstart
```

Don't forget to run `make` each time you modify init.el or update Doom!

Visit the wiki for [a more detailed guide on installing, customizing and
grokking Doom][wiki].

## Feature highlights

+ A fast, organized and opinionated Emacs configuration with a command line
  interface.
+ A custom, declarative [package management system][doom-packages] that combines
  package.el, [use-package] and [quelpa], allowing you to manage packages from
  the command line and install packages from sources other than ELPA.
+ A [popup management system][doom-popups] (powered by [shackle]) that minimizes
  the presence and footprint of temporary and/or disposable buffers.
+ A vim-like experience with [evil-mode], including ports for several vim
  plugins, <kbd>C-x</kbd> omnicompletion and a slew of [custom ex
  commands][doom-my-commands].
+ Integration with [editorconfig]. Let someone else argue about tabs and spaces.
  (spaces, duh).
+ Code completion for many languages, powered by [company-mode] (some languages
  may have external dependencies).
+ Project-awareness powered by [projectile], with tools and an API to navigate
  and manage projects and their files.
+ Fast project search (and replace) utilities, powered by [the_silver_searcher],
  [ripgrep] and [wgrep], with integration for [ivy] (the default), [helm] and
  ido.
+ Isolated and persistent workspaces powered by [persp-mode]. Also substitutes
  for vim tabs.
+ Inline/live code evaluation (using [quickrun]), including REPLs for a variety
  of languages.

## Troubleshooting

Found a problem? Here are some things to try:

+ Run `make install` to ensure all plugins are installed.
+ `void-function` or `void-variable` errors could signal an out-of-date
  autoloads file. Run `make autoloads` or `M-x doom//reload-autoloads` to update
  it.
+ Scan for common OS/environment issues with `make doctor`.
+ **Never debug byte-compiled code. It will interfere in subtle ways.** Clean up
  \*.elc files with `make clean` or `M-x doom//clean-byte-compiled-files`.
+ Check [the FAQ][wiki-troubleshooting] to see if your issue is mentioned.
+ Check the relevant module's README.org, if one exists. There may be extra
  steps to getting certain features to work.

If all else has failed, [file a bug report][doom-new-issue].

## Contribute

Doom (and my Emacs work in general) is a labor of love and incurable madness,
done on my spare time. It wasn't intended for public use, but I enjoy making
Doom a resource for others.

If you'd like to support my efforts, I welcome contributions of any kind:

+ I love pull requests and bug reports. Elisp pointers are especially welcome.
  Seriously, don't hesitate to [tell me my Elisp-fu sucks][doom-new-issue]!
+ Talk to me about Emacs workflow, ideas or tooling. Or talk to me about
  gamedev, or pixel art, or anime, or programming, or the weather, or band camp.
  Whatever. I don't mind. Holler at henrik@lissner.net.


[wiki]: /../../wiki
[wiki-conventions]: /../../wiki/Conventions
[wiki-modules]: /../../wiki/Modules
[wiki-customization]: /../../wiki/Customization
[wiki-troubleshooting]: /../../wiki/FAQ#troubleshooting

[doom-my-bindings]: modules/private/hlissner/+bindings.el
[doom-my-commands]: modules/private/hlissner/+commands.el
[doom-new-issue]: https://github.com/hlissner/doom-emacs/issues/new
[doom-packages]: core/autoload/packages.el
[doom-popups]: modules/ui/popup
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

