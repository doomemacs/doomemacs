;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; Major modes
(package! pip-requirements :pin "216cd1690f80cc965d4ae47b8753fc185f778ff6")
(when (featurep! +cython)
  (package! cython-mode :pin "fcfd16c7467c31f255287a73f36cf66b32bc096c")
  (when (featurep! :checkers syntax)
    (package! flycheck-cython :pin "ecc4454d35ab5317ab66a04406f36f0c1dbc0b76")))

;; LSP
(when (featurep! +lsp)
  (unless (featurep! :tools lsp +eglot)
    (if (featurep! +pyright)
        (package! lsp-pyright :pin "9603dda12afaae9c82608c7d3762f98b24b8563f")
      (package! lsp-python-ms :pin "a884a9a4eb1a3acd3d70c776aec5e968bbdc1731"))))

;; Programming environment
(package! anaconda-mode :pin "73266a48fa964d44268c3f3478597e553b9843f1")
(when (featurep! :completion company)
  (package! company-anaconda :pin "da1566db41a68809ef7f91ebf2de28118067c89b"))

;; Environment management
(package! pipenv :pin "b730bb509e8b60af9f5ab1f1e6c3458d1d95d789")
(package! pyvenv :pin "861998b6d157ae73b829f02a5a6c8a9118310831")
(when (featurep! +pyenv)
  (package! pyenv-mode :pin "d191037fe62ed8d4fee5888845da3e2c386d8e89"))
(when (featurep! +conda)
  (package! conda :pin "9f7eea16e9ad3eb34fe3d1cbd9d6162b8046c2f8"))
(when (featurep! +poetry)
  (package! poetry :pin "22a76cdcba180b4689a6b45c97669e3c76cd36ed"))

;; Testing frameworks
(package! nose
  ;; REVIEW Remove this when emacsmirror/epkgs updates its emacsattic index
  :recipe (:host github :repo "emacsattic/nose")
  :pin "f8528297519eba911696c4e68fa88892de9a7b72")
(package! python-pytest :pin "fc056faf2757c42641ed94d36a090e56eb13572f")

;; Import managements
(package! pyimport :pin "a6f63cf7ed93f0c0f7c207e6595813966f8852b9")
(package! py-isort :pin "e67306f459c47c53a65604e4eea88a3914596560")
