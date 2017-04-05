# Modules

Modules are made up of four parts, **all of which are optional**:

```text
modules/category/submodule/
modules/category/submodule/config.el
modules/category/submodule/packages.el
modules/category/submodule/autoload.el
modules/category/submodule/autoload/*.el
```

## config.el

The module's main configuration file. It is the first file loaded when the
module is loaded (through `doom!` or `require!`).

## packages.el

Where module's tell DOOM what packages to install and where to get them from.
These should be _pure/declarative and idempotent_, and shouldn't have any
side-effects (besides altering the `doom-modules` and `doom-packages`
variables), and should have deterministic results when evaluated.

By default, packages are retrieved from ELPA. Otherwise, a MELPA-style recipe
plist can be used to fetch it from elsewhere:

```emacs-lisp
;; from modules/tools/rotate-text/packages.el
(package! rotate-text :recipe (:fetcher github :repo "debug-ito/rotate-text.el"))
```

Other modules' packages.el files can be depended on, through `depends-on!`:

```emacs-lisp
;; from modules/feature/file-templates/packages.el
(depends-on! :feature snippets)
```

## autoload.el OR autoload/*.el

These are scanned by `doom/reload-autoloads`, whose functions are lazily-loaded,
given that they're marked with an `;;;###autoload` cookie:

```emacs-lisp
;; from modules/lang/org/autoload/org.el
;;;###autoload
(defun +org/toggle-checkbox ()
  (interactive)
  [...])

;; from modules/lang/org/autoload/evil.el
;;;###autoload (autoload '+org:attach "lang/org/autoload/evil" nil t)
(evil-define-command +org:attach (&optional uri)
  (interactive "<a>")
  [...])
```

## Other files

My convention for extra configuration files is a `+` prefix, e.g.
`modules/feature/version-control/+git.el`. These are **not** automatically
loaded, and must be loaded manually with `load!` from a module's `config.el`:

```emacs-lisp
;; from modules/feature/version-control/config.el
(load +git)
```

----

# What modules aren't

Modules loosely take after Spacemacs' notion of layers, but are not intended to
be interchangeable. Their purpose is _almost_ purely organizational.

Use `featurep!` to check for other modules:

```emacs-lisp
;; from modules/lang/go/packages.el
(when (featurep! :completion company)
  (package! company-go))

;; from modules/lang/go/config.el
(def-package! company-go
  :when (featurep! :completion company)
  [...])
```
