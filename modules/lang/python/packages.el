;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; Major modes
(package! pip-requirements :pin "216cd1690f80cc965d4ae47b8753fc185f778ff6")
(when (featurep! +cython)
  (package! cython-mode :pin "21b4e533747507c4dad5e6908df1f157dff4de89")
  (when (featurep! :checkers syntax)
    (package! flycheck-cython :pin "ecc4454d35ab5317ab66a04406f36f0c1dbc0b76")))

;; LSP
(when (featurep! +lsp)
  (unless (featurep! :tools lsp +eglot)
    (if (featurep! +pyright)
        (package! lsp-pyright :pin "d9e7d3cf1bdc8ee0395c8df4408b05ee3ba9c22a")
      (package! lsp-python-ms :pin "c4ebc7a11398733055a1dc07f9cffacd04d1c2dc"))))

;; Programming environment
(package! anaconda-mode :pin "39b1cf88c8c459901630d248d6135d8644075648")
(when (featurep! :completion company)
  (package! company-anaconda :pin "da1566db41a68809ef7f91ebf2de28118067c89b"))

;; Environment management
(package! pipenv :pin "b730bb509e8b60af9f5ab1f1e6c3458d1d95d789")
(package! pyvenv :pin "0bf4b87068e9040fc99afa85af1992247dd7aacd")
(when (featurep! +pyenv)
  (package! pyenv-mode :pin "b818901b8eac0e260ced66a6a5acabdbf6f5ba99"))
(when (featurep! +conda)
  (package! conda :pin "9f7eea16e9ad3eb34fe3d1cbd9d6162b8046c2f8"))
(when (featurep! +poetry)
  (package! poetry :pin "d5163fe065239bb7b46ed8b3ff3b85b1f3229af3"))

;; Testing frameworks
(package! nose
  ;; REVIEW Remove this when emacsmirror/epkgs updates its emacsattic index
  :recipe (:host github :repo "emacsattic/nose")
  :pin "f8528297519eba911696c4e68fa88892de9a7b72")
(package! python-pytest :pin "4a1c4c8915c12e540d41aae1d4e326a2362da541")

;; Import managements
(package! pyimport :pin "a6f63cf7ed93f0c0f7c207e6595813966f8852b9")
(package! py-isort :pin "e67306f459c47c53a65604e4eea88a3914596560")
