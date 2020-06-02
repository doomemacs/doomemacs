;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; Major modes
(package! pip-requirements :pin "216cd1690f")
(when (featurep! +cython)
  (package! cython-mode :pin "48dc1f0169")
  (when (featurep! :checkers syntax)
    (package! flycheck-cython :pin "ecc4454d35")))

;; LSP
(when (and (featurep! +lsp) (not (featurep! :tools lsp +eglot)))
  (package! lsp-python-ms :pin "5d0c799099"))

;; Programming environment
(package! anaconda-mode :pin "10299bd9ff")
(when (featurep! :completion company)
  (package! company-anaconda :pin "a31354ca8e"))

;; Environment management
(package! pipenv :pin "b730bb509e")
(package! pyvenv :pin "861998b6d1")
(when (featurep! +pyenv)
  (package! pyenv-mode :pin "aec6f2aa28"))
(when (featurep! +conda)
  (package! conda :pin "335474e409"))
(when (featurep! +poetry)
  (package! poetry :pin "6dcc9d22ca"))

;; Testing frameworks
(package! nose
  ;; REVIEW Remove this when emacsmirror/epkgs updates its emacsattic index
  :recipe (:host github :repo "emacsattic/nose")
  :pin "f8528297519eba911696c4e68fa88892de9a7b72")
(package! python-pytest :pin "09ad688df207ee9b02c990d3897a9e2841931d97")

;; Import managements
(package! pyimport :pin "a6f63cf7ed")
(package! py-isort :pin "e67306f459")
