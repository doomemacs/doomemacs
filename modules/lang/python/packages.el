;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; Major modes
(package! pip-requirements :pin "31e0dc62abb2d88fa765e0ea88b919d756cc0e4f")
(when (modulep! +cython)
  (package! cython-mode :pin "3e4790559d3168fe992cf2aa62f01423038cedb5")
  (when (modulep! :checkers syntax)
    (package! flycheck-cython :pin "ecc4454d35ab5317ab66a04406f36f0c1dbc0b76")))

;; LSP
(when (modulep! +lsp)
  (unless (modulep! :tools lsp +eglot)
    (when (modulep! +pyright)
      (package! lsp-pyright :pin "0c0d72aedd18b16f48379b7d2f9ecb9c068713b0"))))

;; Programming environment
(package! anaconda-mode :pin "f900bd7656a03aa24ef3295251f266736f7756eb")
(when (modulep! :completion company)
  (package! company-anaconda :pin "169252fca79a79da41ef22f2ec0eab0cf1313966"))

;; Environment management
(package! pipenv :pin "3af159749824c03f59176aff7f66ddd6a5785a10")
(package! pyvenv :pin "31ea715f2164dd611e7fc77b26390ef3ca93509b")
(when (modulep! +pyenv)
  (package! pyenv-mode :pin "76787ea44f633426df402341663784db3a9e9639"))
(when (modulep! +conda)
  (package! conda :pin "ce748a53f9c7d7a7d112632d32c848d6e5482e18"))
(when (modulep! +poetry)
  (package! poetry :pin "1dff0d4a51ea8aff5f6ce97b154ea799902639ad"))

;; Testing frameworks
(package! nose :pin "f8528297519eba911696c4e68fa88892de9a7b72")
(package! python-pytest :pin "dcdaec6fe203f08bda0f5ee1931370dfd075a4ff")

;; Import managements
(package! pyimport :pin "4398ce8dd64fa0f685f4bf8683a35087649346d3")
(package! py-isort :pin "e67306f459c47c53a65604e4eea88a3914596560")
