<a href="http://doomemacs.org">
  <img src="https://img.shields.io/github/tag/hlissner/doom-emacs.svg?label=release&color=orange&style=for-the-badge"  alt="Made with Doom Emacs">
</a>
<a href="https://emacs.org">
  <img src="https://img.shields.io/badge/Supports-26.1_--_27.0.50-blueviolet.svg?style=for-the-badge&logo=GNU%20Emacs&logoColor=white" alt="Supports Emacs 26.x - 27.0.50">
</a>
<a href="https://github.com/hlissner/doom-emacs/actions">
  <img src="https://github.com/hlissner/doom-emacs/workflows/CI/badge.svg" alt="Build status: develop">
</a>
<a href="https://discord.gg/qvGgnVx">
  <img src="https://img.shields.io/badge/Discord-blue.svg?logo=discord&label=join&style=for-the-badge" alt="Join our discord server" align="right">
</a>
<br><br>

![Doom Emacs Screenshot](https://raw.githubusercontent.com/hlissner/doom-emacs/screenshots/main.png)

<p align="center">
  <b><a href="/../../tree/screenshots">Screenshots</a></b>
  |
  <b><a href="docs/getting_started.org">Get started</a></b>
  |
  <b><a href="docs/contributing.org">Contribute</a></b>
  |
  <b><a href="docs/index.org">Documentation</a></b>
  |
  <b><a href="docs/faq.org">FAQ</a></b>
</p>

---

**Quick start**

1. **Install Emacs 26.1+**. 27 is recommended. _28+ is not supported_.
2. Install [ripgrep](https://github.com/BurntSushi/ripgrep) 11.0+.
3. Windows and BSD users will need GNU Find.
4. Clone Doom and run its installer:

   ```bash
   git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
   ~/.emacs.d/bin/doom install
   ```

Find more detailed install instructions [in the
documentation](docs/getting_started.org#install).

**Table of Contents**

- [What is Doom Emacs](#what-is-doom-emacs)
    - [Doom's mantras](#dooms-mantras)
    - [Features](#features)
- [Getting Help](#getting-help)
    - [Community](#community)
    - [Troubleshooting](#troubleshooting)
    - [Contributing](#contributing)

# What is Doom Emacs

<a href="http://ultravioletbat.deviantart.com/art/Yay-Evil-111710573">
  <img src="https://github.com/hlissner/doom-emacs/raw/screenshots/cacochan.png" align="right" />
</a>

It is a story as old as time. A stubborn, shell-dwelling, and melodramatic
vimmer -- envious of the features of modern text editors -- spirals into despair
before succumbing to the [dark side][url:evil-mode]. This is his config.

Doom is a configuration framework for [GNU
Emacs](https://www.gnu.org/software/emacs/) tailored for Emacs bankruptcy
veterans who want less framework in their frameworks and the performance of a
hand rolled config (or better). It can be a foundation for your own config or a
resource for Emacs enthusiasts to learn more about our favorite OS.

## Doom's mantras

- **Gotta go fast.** Startup and run-time performance are priorities. Doom goes
  beyond lazy loading packages by modifying them to be snappier and load lazier!
- **Close to metal.** There's less between you and vanilla Emacs by design.
  There's less to grok, on top of Emacs.
- **Readability counts.** Internals ought to be written as if reading them were
  part of the user experience, and it is! Modules should be syntactically sweet.
  Backend logic should be functional (as much as elisp permits), abstraction
  light and (hopefully) documented.
- **Opinionated, but not stubborn.** Doom is a bundle of reasonable defaults and
  curated opinions, but all of it should be optional. Use as little or as much
  of it as you like.
- **Your system, your rules.** There are more ways to set up your development
  environment than there are dislikes on Youtube Rewind '18, so Doom leaves it
  to you. Doom will not *automatically* install system dependencies (and will
  coerce its plugins not to do so either). Use `doom doctor` to figure out
  what's missing.

## Features

- Minimalistic good looks inspired by modern editors.
- A modular architecture that can be extended to your own configs.
- A standard library suited to simplifying your config.
- A declarative [package management system][doom:packages] (powered by
  [straight.el][url:straight]) with a command line interface. Install packages
  from anywhere, not just (M)ELPA.
- (Optional) Vim-emulation powered by [evil-mode][url:evil-mode], including
  ports of popular vim plugins and functionality.
- Curated and sane defaults for many packages, (major) OSes, and Emacs itself.
- Support for *many* programming languages. Includes syntax highlighting,
  linters/checker integration, inline code evaluation, code completion (where
  possible), REPLs, documentation lookups, snippets, and more!
- Support for *many* tools, like docker, pass, ansible, terraform, and more.
- A Spacemacs-esque [keybinding scheme][doom:bindings], centered around leader
  and localleader prefix keys (<kbd>SPC</kbd> and <kbd>SPC</kbd><kbd>m</kbd>, by
  default).
- A rule-based [popup management system][doom:popups] to control how temporary
  or disposable buffers are displayed (and disposed of).
- Automatic indentation detection and [editorconfig][url:editorconfig]
  integration. Let someone else argue about tabs vs **\_\***spaces**\*\_**.
- Project-management tools and framework-specific minor modes with their own
  snippets libraries.
- Project search (and replace) utilities, powered by [ripgrep][url:ripgrep].
- Isolated and persistent workspaces (also substitutes for vim tabs).
- An envvar file generator that captures a snapshot of your shell environment
  for Doom to load at startup. No more struggling to get Emacs to inherit your
  `PATH`, among other things.

# Getting Help

## Community

We have [a Discord server][url:discord]! Hop on and say hi!

## Troubleshooting

Encountered a problem? Here are some things to try before shooting off that bug
report:

- Run `bin/doom sync`. This ensures Doom is properly set up and its autoloads
  files are up-to-date.
- Folks who have byte-compiled their config (with `bin/doom compile`) should run
  `bin/doom clean` to rule out stale bytecode. Never debug with a byte-compiled
  config. It makes your job harder.
- Run `bin/doom doctor` to detect common issues in your development environment
  and missing third party dependencies.
- Search [Doom's issue tracker][github:issues] in case your issue was already
  reported.
- [Visit our FAQ][docs:faq] to see if your issue is listed.

If all else fails, [file that bug report][github:new-issue]! **Please do not
ignore the issue template!** It's a great help if you can [include a backtrace
with errors][docs:backtrace].

## Contributing

Doom (and my Emacs work in general) is a labor of love and incurable madness,
done on my spare time. If you'd like to support my work, there are many things
you can do to help. I welcome contributions!

- I love pull requests and bug reports. Check out the [Contributing
  Guidelines][docs:contributing] to find out how you can help out.
- I welcome Elisp pointers! Don't hesitate to [tell me my Elisp-fu
  sucks][github:new-issue] (but please tell me why).
- Hop on [our Discord server][url:discord] and say hi! Help others out, hang out
  or talk to me about Emacs, or gamedev, or programming, machine learning,
  physics, pixel art, anime, gaming -- anything you like. Nourish this lonely
  soul!
- If you'd like to support my work financially, consider buying me a drink
  through [liberapay][url:liberapay] or [paypal][url:paypal]. Donations are a
  great help. My work here contends with studies, ventures in indie gamedev, and
  my freelance work.


[docs:wiki]: docs/index.org
[docs:wiki-quickstart]: docs/getting_started.org
[docs:wiki-modules]: docs/index.org#Module%20List
[docs:wiki-customization]: docs/getting_started.org#Customize
[docs:contributing]: docs/contributing.org
[docs:faq]: docs/faq.org
[docs:backtrace]: https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#how-to-extract-a-backtrace-from-an-error
[github:new-issue]: https://github.com/hlissner/doom-emacs/issues/new
[github:issues]: https://github.com/hlissner/doom-emacs/issues
[doom:bindings]: modules/config/default/+evil-bindings.el
[doom:packages]: core/autoload/packages.el
[doom:popups]: modules/ui/popup/README.org
[url:discord]: https://discord.gg/qvGgnVx
[url:liberapay]: https://liberapay.com/hlissner/donate
[url:paypal]: https://paypal.me/henriklissner/10
[url:editorconfig]: http://editorconfig.org/
[url:evil-mode]: https://github.com/emacs-evil/evil
[url:ripgrep]: https://github.com/BurntSushi/ripgrep
[url:straight]: https://github.com/raxod502/straight.el
