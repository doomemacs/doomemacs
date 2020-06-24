;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; Major modes
(package! pip-requirements :pin "216cd1690f80cc965d4ae47b8753fc185f778ff6")
(when (featurep! +cython)
  (package! cython-mode :pin "12ab8bc0056a0e77ccc0756955eb1621fd3b35db")
  (when (featurep! :checkers syntax)
    (package! flycheck-cython :pin "ecc4454d35ab5317ab66a04406f36f0c1dbc0b76")))

;; LSP
(when (and (featurep! +lsp)
           (not (featurep! :tools lsp +eglot)))
  (package! lsp-python-ms :pin "7068cf04a0d0a1877afe56990cc577edd824a1e4"))

;; Programming environment
(package! anaconda-mode :pin "10299bd9ff38c4f0da1d892905d02ef828e7fdce")
(when (featurep! :completion company)
  (package! company-anaconda :pin "da1566db41a68809ef7f91ebf2de28118067c89b"))

;; Environment management
(package! pipenv :pin "b730bb509e8b60af9f5ab1f1e6c3458d1d95d789")
(package! pyvenv :pin "861998b6d157ae73b829f02a5a6c8a9118310831")
(when (featurep! +pyenv)
  (package! pyenv-mode :pin "d191037fe62ed8d4fee5888845da3e2c386d8e89"))
(when (featurep! +conda)
  (package! conda :pin "9d0213020ff170b17e11b35cad40ac9a8bf30f4c"))
(when (featurep! +poetry)
  (package! poetry :pin "d876522e5af576d53c62b2838f85c9441fe62258"))

;; Testing frameworks
(package! nose
  ;; REVIEW Remove this when emacsmirror/epkgs updates its emacsattic index
  :recipe (:host github :repo "emacsattic/nose")
  :pin "f8528297519eba911696c4e68fa88892de9a7b72")
(package! python-pytest :pin "09ad688df207ee9b02c990d3897a9e2841931d97")

;; Import managements
(package! pyimport :pin "a6f63cf7ed93f0c0f7c207e6595813966f8852b9")
(package! py-isort :pin "e67306f459c47c53a65604e4eea88a3914596560")
