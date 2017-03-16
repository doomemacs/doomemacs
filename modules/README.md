# Modules

Modules are made up of three (optional) parts:

+ A `config.el` file, automatically loaded when the module is loaded
  (through `doom!` or `require!`). It uses `def-package!` calls (thin
  wrappers around `use-package`) to configure packages.
+ A `packages.el` file filled with `package!` and `depends-on!`
  declarations. These are purely declarative macros. They populate
  `doom-packages` and `doom-modules` for the package management
  system.
+ Either an `autoload.el` file or `autoload/*.el` files, which are
  scanned by `doom/reload-autoloads` and lazily loaded.

The convention for extra config files is to prefix them with a plus
(`+git.el`). These must be manually loaded using `load!` from a
module's configuration.

## What modules aren't

Modules loosely take after Spacemacs' notion of layers, but were not
meant to be as interchangeable. Their purpose is _almost_ purely
organizational.

The only exception are completion modules. Other modules make no
assumptions about which completion modules are enabled. If company
isn't installed, company plugins will silently refuse to install and
their respective `def-package!` blocks will be ignored.
