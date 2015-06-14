# NARF Emacs

![Screenshot](screenshots/01.png)

> What we do every night, Pinky...

This is emacs for the stubborn vimmer, megalomaniac mouse and/or masochists
alike. It has been configured first: to emulate vim as best it can, and second:
to surpass it in any way possible; all this is built on top of
[Evil-mode](https://gitorious.org/evil/pages/Home), a vim emulator for emacs.

This has only been tested on Emacs 24.5 on OSX. YMMV.

Any contributions or suggestions are welcome. The world won't take over itself.

## Installation

`brew install cask`

Narf uses [Cask](https://github.com/cask/cask) to manage plugins.

Also, though not strictly a requirement, I recommend the railwaycat/emacsmacport
build of emacs for OSX, which you can get via homebrew:

```sh
brew tap railwaycat/emacsmacport
brew install emacs-mac --HEAD --use-git-head --with-imagemagick --with-modern-icon
```

A recursive clone of this repo will get you all you need.

```
git clone --recursive https://github.com/hlissner/emacs.d ~/.emacs.d
cd ~/.emacs.d
make          # installs plugins via cask and generates autoloads
make compile  # optionally byte-compiles everything
```

## Features

A summary of what to expect can be found in these three files:

```
./Cask                     # what packages are used (and where)
./init.el                  # lists all core files and modules loaded
./private/my-bindings.el   # the keybindings
./private/my-commands.el   # what custom ex commands are defined
```

## What about Windo-
![Windows, you say...](http://i3.kym-cdn.com/photos/images/newsfeed/000/549/293/504.gif)
