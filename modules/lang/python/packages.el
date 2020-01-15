;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; Major modes
(package! pip-requirements :pin "216cd1690f80cc965d4ae47b8753fc185f778ff6")
(when (featurep! +cython)
  (package! cython-mode :pin "6d2c3b9372547ce0aefac2babfe48dc1568875b9")
  (when (featurep! :checkers syntax)
    (package! flycheck-cython :pin "ecc4454d35ab5317ab66a04406f36f0c1dbc0b76")))

;; LSP
(when (featurep! +lsp)
  (package! lsp-python-ms :pin "83ef84d9a4a942f8882b00d07bb78b15f716e89d"))

;; Programming environment
(package! anaconda-mode :pin "1bc301b2d2bc336988f4a16a891c275a90136ca5")
(when (featurep! :completion company)
  (package! company-anaconda :pin "398fad19160cc1d0e31dcb1d4a3f88de7a2d355d"))

;; Environment management
(package! pipenv :pin "b730bb509e8b60af9f5ab1f1e6c3458d1d95d789")
(package! pyvenv :pin "861998b6d157ae73b829f02a5a6c8a9118310831")
(when (featurep! +pyenv)
  (package! pyenv-mode :pin "aec6f2aa289f6aed974f053c081143758dd142fb"))
(when (featurep! +conda)
  (package! conda :pin "41e9593cf230a50183a36fa9c0a4853acb2e7505"))

;; Testing frameworks
(package! nose :pin "f8528297519eba911696c4e68fa88892de9a7b72")
(package! python-pytest :pin "09ad688df207ee9b02c990d3897a9e2841931d97")

;; Import managements
(package! pyimport :pin "a6f63cf7ed93f0c0f7c207e6595813966f8852b9")
(package! py-isort :pin "e67306f459c47c53a65604e4eea88a3914596560")
