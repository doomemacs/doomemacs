# NARF Emacs

![Screenshot](assets/screenshots/01.png)

> What we do every night, Pinky...

This is Emacs for stubborn vimmers and megalomaniacal mice alike. Its goals are:
first, to emulate vim as best it can, and second, surpass it in any possible
way. Narf uses [Evil-mode](https://gitorious.org/evil/pages/Home) to emulate
vim, Cask to manage plugins, and a slew of jury-rigged neckbeard shenanigans to
keep my mountain dew cool.

It is tailored for OSX users running Emacs **24.5+**

Any contributions or suggestions are welcome. The world won't take over itself.

## Installation

```
brew install cask
brew install emacs --devel --with-imagemagick --with-librsvg --with-cocoa
git clone --recursive https://github.com/hlissner/emacs.d ~/.emacs.d
cd ~/.emacs.d
make          # installs plugins via cask and generates autoloads
make compile  # optionally byte-compiles everything
```

## Features

You'll get a good picture of what's in here from these files:

```
./Cask                     # what packages are used (and where)
./init.el                  # lists all core files and modules loaded
./private/my-bindings.el   # the keybindings
./private/my-commands.el   # available ex commands
```

## What about Windo-
![Windows, you say...](http://i3.kym-cdn.com/photos/images/newsfeed/000/549/293/504.gif)
