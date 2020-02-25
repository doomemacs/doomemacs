;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; Major modes
(package! pip-requirements :pin "216cd1690f")
(when (featurep! +cython)
  (package! cython-mode :pin "1bc86b5750")
  (when (featurep! :checkers syntax)
    (package! flycheck-cython :pin "ecc4454d35")))

;; LSP
(when (featurep! +lsp)
  (package! lsp-python-ms :pin "83ef84d9a4"))

;; Programming environment
(package! anaconda-mode :pin "1bc301b2d2")
(when (featurep! :completion company)
  (package! company-anaconda :pin "398fad1916"))

;; Environment management
(package! pipenv :pin "b730bb509e")
(package! pyvenv :pin "861998b6d1")
(when (featurep! +pyenv)
  (package! pyenv-mode :pin "aec6f2aa28"))
(when (featurep! +conda)
  (package! conda :pin "814439dffa"))

;; Testing frameworks
(package! nose :pin "f852829751")
(package! python-pytest :pin "09ad688df2")

;; Import managements
(package! pyimport :pin "a6f63cf7ed")
(package! py-isort :pin "e67306f459")
