# NARF Emacs

![Screenshot](screenshots/01.png)

> What we do every night, Pinky...

This is emacs for the stubborn vimmer, megalomaniac mouse and masochists alike.
It has been configured first: to emulate vim as best it can, and second: to
surpass it in any way possible; all this is built on top of
[Evil-mode](https://gitorious.org/evil/pages/Home), a vim emulator for emacs.

This has only been tested on Emacs 24.5 on OSX, YMMV.

Any contributions or suggestions are welcome. The world won't take over itself.

## Installation

`brew install cask`

Narf only requires [Cask](https://github.com/cask/cask) for managing its plugins
outside of emacs.

Also, though not strictly a requirement, I recommend the railwaycat/emacsmacport
build of emacs for OSX users, which you can get via homebrew:

```sh
brew tap railwaycat/emacsmacport
brew install emacs-mac
```

Lastly, a recursive clone will get everything you need:

```
git clone --recursive https://github.com/hlissner/emacs.d ~/.emacs.d
cd ~/.emacs.d
make          # installs plugins via cask and generates autoloads
make compile  # optional
```

## Features

A summary of what to expect can be found in these three files:

```
./Cask                     # what packages are included
./init/narf-commands.el    # what custom ex commands are defined
./init/narf-bindings.el    # the keybindings
```

## Disclaimer

I am not an elisp guru. You have been warned.

## What about Windo-
![Windows, you say...](http://i3.kym-cdn.com/photos/images/newsfeed/000/549/293/504.gif)
