<img src="https://raw.githubusercontent.com/hlissner/doom-emacs/screenshots/main.png" alt="Main screenshot" />

<img src="https://img.shields.io/github/tag/hlissner/doom-emacs.svg?label=release"
     alt="Current release"
     align="left" />
<a href="https://travis-ci.org/hlissner/doom-emacs">
  <img src="https://img.shields.io/travis/hlissner/doom-emacs/master.svg?label=master"
       alt="build status (master)"
       align="left" />
</a>
<a href="https://travis-ci.org/hlissner/doom-emacs">
  <img src="https://img.shields.io/travis/hlissner/doom-emacs/master.svg?label=develop"
       alt="build status (develop)"
       align="left" />
</a>
<a href="https://discord.gg/bcZ6P3y">
  <img src="https://img.shields.io/badge/Discord-blue.svg?logo=discord"
       alt="Discord Server"
       align="left" />
</a>

<p align="center">
  <a href="/../../wiki">wiki</a>&nbsp; |&nbsp;
  <a href="/../../tree/screenshots">screenshots</a>&nbsp; |&nbsp;
  <a href="/../../faq.org">faq</a>&nbsp; |&nbsp;
  <a href="/../../wiki/FAQ#troubleshooting">troubleshooting</a>
  <!--a href="CHANGELOG.org">changelog</a-->
</p>

- - -

Quick start
-----------

```bash
git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom quickstart
```

> Doom supports Emacs 25.3 and newer, but **Emacs 26.1 is recommended.** Doom
> works best on Linux & MacOS. Your mileage may vary on Windows.


Table of Contents
==================
- [What is Doom Emacs](#what-is-doom-emacs)
    - [Doom's mantras](#dooms-mantras)
    - [Feature highlights](#feature-highlights)
- [Getting Help](#getting-help)
- [Contributing](#contributing)


What is Doom Emacs
==================

<a href="http://ultravioletbat.deviantart.com/art/Yay-Evil-111710573">
  <img src="https://github.com/hlissner/doom-emacs/raw/screenshots/cacochan.png" align="right" />
</a>

It is a story as old as time. A stubborn, shell-dwelling, and melodramatic
vimmer -- envious of the features of modern text editors -- spirals into despair
before succumbing to the [dark side][url:evil-mode]. This is his config.

Doom is a configuration for [GNU Emacs](https://www.gnu.org/software/emacs/). It
can be used as framework for your own configuration, or as a resource for fellow
Emacs enthusiasts who want to learn more about our favorite OS.

Doom's mantras
--------------
- **Gotta go fast.** Startup and runtime speed are high priorities; many
  expensive, heavy-handed features and packages have been fine-tuned to that
  end.
- **Hacker-friendly.** Doom caters to the command line denizen unafraid of
  writing a little (or a lot of) code to tailor their editor. It also inherits
  your shell configuration, warts 'n all, and expects frequent trips into the
  terminal to manage Doom with its `bin/doom` utility.
- **Opinionated, but not stubborn.** Doom has _many_ opinions spread out across
  its 120+ modules designed to iron out idiosynchrosies and provide a better and
  more consistent baseline experience of Emacs and its plugins. However, they
  mustn't ever compromise your ability to change, rewrite or disable any or all
  of it, if you ask nicely.
- **Written to be read.** Doom's source ought to be self documenting and easy to
  grok. Modules should be syntactically sweet and concise, and backend logic
  should be explicit and abstraction-light. Where complexity arises, comments
  and documentation shouldn't be far away.

Feature Highlights
------------------
- A declarative [package management system][doom:packages] with a command line
  interface that combines package.el, [use-package] and [quelpa], allowing you
  to install packages from anywhere.
- A [popup management system][doom:popups] with customizable rules to dictate
  how temporary/disposable buffers are displayed.
- A vim-centric (and optional) experience with [evil-mode][url:evil-mode],
  including ports of several popular vim plugins, <kbd>C-x</kbd> omnicompletion
  and a slew of [custom ex commands][doom:commands].
- A Spacemacs-esque [keybinding scheme][doom:bindings], centered around leader
  and localleader prefix keys (<kbd>SPC</kbd> and <kbd>SPC</kbd><kbd>m</kbd>, by
  default).
- Indentation detection and optional integration with
  [editorconfig][url:editorconfig]. Let someone else argue about tabs vs
  ___***spaces***___.
- Code completion for many languages, powered by
  [company-mode][url:company-mode] (some may have external dependencies).
- Project-awareness powered by [projectile][url:projectile], with tools and an
  API to navigate and manage projects, as well as project/framework-specific
  minor modes and snippets libraries (and the ability to define your own).
- Project search (and replace) utilities, powered by
  [the_silver_searcher][url:ag], [ripgrep][url:rg], git-grep and
  [wgrep][url:wgrep], with integration for [ivy][url:ivy] (the default) and
  [helm][url:helm].
- Isolated and persistent workspaces powered by [persp-mode][url:persp-mode].
  Also substitutes as vim tabs.
- Inline/live code evaluation (using [quickrun][url:quickrun]), with REPL
  support for a variety of languages.
- A jump-to-definition/references implementation for all languages that tries to
  "just work," resorting to mode-specific functionality, before falling back on
  [dump-jump][url:dumb-jump].


Troubleshooting
===============

Encountered strange behavior or an error? Here are some things to try before you
shoot off that bug report:

- Run `bin/doom refresh`. This ensures Doom is properly set up and its autoloads
  files are up-to-date.
- If you have byte-compiled your config (with `bin/doom compile`), see if
  `bin/doom clean` makes your issue go away. Never debug issues with a
  byte-compiled config, it will only make your job harder.
- Run `bin/doom doctor` to detect common issues in your development environment.
- Search Doom's issue tracker for mention of any error messages you've received.
- [Visit our FAQ][docs:faq] to see if your issue is listed.

If all else fails, [file that bug report][github:new-issue]! Please include the
behavior you've observed, the behavior you expected, and any error message in
the \*Messages\* buffer (can be opened with <kbd>SPC h m</kbd> or `M-x
view-echo-area-messages`). It'd be a great help if you included a backtrace with
them as well.

We've also got [a Discord server][url:discord]. Hop on! We can help!


Contributing
============

Doom (and my Emacs work in general) is a labor of love and incurable madness,
done on my spare time. If you'd like to support my work, I welcome
contributions:

- I love pull requests and bug reports. Check out the [Contributing
  Guidelines][docs:contributing] (WIP) to find out how you can help out.
- I welcome Elisp pointers! Don't hesitate to [tell me my Elisp-fu
  sucks][github:new-issue] (but please tell me why).
- Hop on [our Discord server][url:discord] and say hi! Help others out, hang out
  or talk to me about Emacs, or gamedev, or programming, machine learning,
  physics, pixel art, anime, gaming -- anything you like. Nourish this lonely
  soul!
- If you'd like to support my work financially, consider buying me a drink
  through [liberapay][url:liberapay] or [paypal][url:paypal]. Donations are a
  great help. My work here contends with full-time studies, my ventures in indie
  gamedev, and my freelance work.


<!-- [docs:wiki]: docs/index.org -->
<!-- [docs:wiki-quickstart]: docs/getting-started.org -->
<!-- [docs:wiki-modules]: docs/modules.org -->
<!-- [docs:wiki-customization]: docs/customize.org -->
<!-- [docs:contributing]: docs/contribute.org -->
<!-- [docs:faq]: docs/faq.org -->
[docs:faq]: /../../wiki/FAQ

[github:new-issue]: https://github.com/hlissner/doom-emacs/issues/new
[doom:bindings]: modules/config/default/+bindings.el
[doom:commands]: modules/config/default/+evil-commands.el
[doom:packages]: core/autoload/packages.el
[doom:popups]: modules/feature/popup/README.org

[url:discord]: https://discord.gg/bcZ6P3y
[url:liberapay]: https://liberapay.com/hlissner/donate
[url:paypal]: https://paypal.me/henriklissner/10

[url:company-mode]: https://github.com/company-mode/company-mode
[url:doom-themes]: https://github.com/hlissner/emacs-doom-themes
[url:editorconfig]: http://editorconfig.org/
[url:evil-mode]: https://github.com/emacs-evil/evil
[url:helm]: https://github.com/emacs-helm/helm
[url:ivy]: https://github.com/abo-abo/swiper
[url:persp-mode]: https://github.com/Bad-ptr/persp-mode.el
[url:projectile]: https://github.com/bbatsov/projectile
[url:quelpa]: https://github.com/quelpa/quelpa
[url:quickrun]: https://github.com/syohex/emacs-quickrun
[url:ripgrep]: https://github.com/BurntSushi/ripgrep
[url:the_silver_searcher]: https://github.com/ggreer/the_silver_searcher
[url:use-package]: https://github.com/jwiegley/use-package
[url:wgrep]: https://github.com/mhayashi1120/Emacs-wgrep
