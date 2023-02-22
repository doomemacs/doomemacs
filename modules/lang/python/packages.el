;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; Major modes
(package! pip-requirements :pin "216cd1690f80cc965d4ae47b8753fc185f778ff6")
(when (modulep! +cython)
  (package! cython-mode :pin "3e4790559d3168fe992cf2aa62f01423038cedb5")
  (when (modulep! :checkers syntax)
    (package! flycheck-cython :pin "ecc4454d35ab5317ab66a04406f36f0c1dbc0b76")))

;; LSP
(when (modulep! +lsp)
  (unless (modulep! :tools lsp +eglot)
    (if (modulep! +pyright)
        (package! lsp-pyright :pin "4cd2adbb32287278d9d9da59a3212a53ecdf8036")
      (package! lsp-python-ms :pin "f8e7c4bcaefbc3fd96e1ca53d17589be0403b828"))))

;; Programming environment
(package! anaconda-mode :pin "ca8edbaa7662d97e4a4416ec9a8d743863303911")
(when (modulep! :completion company)
  (package! company-anaconda :pin "da1566db41a68809ef7f91ebf2de28118067c89b"))

;; Environment management
(package! pipenv :pin "3af159749824c03f59176aff7f66ddd6a5785a10")
(package! pyvenv :pin "31ea715f2164dd611e7fc77b26390ef3ca93509b")
(when (modulep! +pyenv)
  (package! pyenv-mode :pin "b818901b8eac0e260ced66a6a5acabdbf6f5ba99"))
(when (modulep! +conda)
  (package! conda :pin "28f51e49fd25abff14c1b46dea196a90a77ced64"))
(when (modulep! +poetry)
  (package! poetry :pin "5a8575bc78d7fc5a39b2ba4665a909e36961fceb"))

;; Testing frameworks
(package! nose :pin "f8528297519eba911696c4e68fa88892de9a7b72")
(package! python-pytest :pin "33c921adaa6c9c8f7cceba2342114c6b406e0d7c")

;; Import managements
(package! pyimport :pin "c006a5fd0e5c9e297aa2ad71b2f02f463286b5e3")
(package! py-isort :pin "e67306f459c47c53a65604e4eea88a3914596560")
