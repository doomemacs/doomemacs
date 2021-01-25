;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; Major modes
(package! pip-requirements :pin "216cd1690f80cc965d4ae47b8753fc185f778ff6")
(when (featurep! +cython)
  (package! cython-mode :pin "17e03b8658a07b6d6da49300b39b57ed9c59ddb1")
  (when (featurep! :checkers syntax)
    (package! flycheck-cython :pin "ecc4454d35ab5317ab66a04406f36f0c1dbc0b76")))

;; LSP
(when (featurep! +lsp)
  (unless (featurep! :tools lsp +eglot)
    (if (featurep! +pyright)
        (package! lsp-pyright :pin "71ff088ac4c93b0edd012f305a3dfd1602c5d21e")
      (package! lsp-python-ms :pin "5470ada6cde6e68fe6ce13ff1146c89c3bae0cc8"))))

;; Programming environment
(package! anaconda-mode :pin "b1875a5d0ec9885c1c42558c126b93ee6bcedaa6")
(when (featurep! :completion company)
  (package! company-anaconda :pin "da1566db41a68809ef7f91ebf2de28118067c89b"))

;; Environment management
(package! pipenv :pin "f516a1a8677a6a1ce9683056e9f77b1e785e8431")
(package! pyvenv :pin "9b3678bc29192d2dba64df90fbdb17393ef8d877")
(when (featurep! +pyenv)
  (package! pyenv-mode :pin "b818901b8eac0e260ced66a6a5acabdbf6f5ba99"))
(when (featurep! +conda)
  (package! conda :pin "dce431b25f5a13af58cc7cacfa7968b5a888609c"))
(when (featurep! +poetry)
  (package! poetry :pin "d5163fe065239bb7b46ed8b3ff3b85b1f3229af3"))

;; Testing frameworks
(package! nose
  ;; REVIEW Remove this when emacsmirror/epkgs updates its emacsattic index
  :recipe (:host github :repo "emacsattic/nose")
  :pin "f8528297519eba911696c4e68fa88892de9a7b72")
(package! python-pytest :pin "3fadf1f8bc363d57c54eedd1bf98e6d9db9f0a62")

;; Import managements
(package! pyimport :pin "a6f63cf7ed93f0c0f7c207e6595813966f8852b9")
(package! py-isort :pin "e67306f459c47c53a65604e4eea88a3914596560")
