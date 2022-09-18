;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; Major modes
(package! pip-requirements :pin "216cd1690f80cc965d4ae47b8753fc185f778ff6")
(when (modulep! +cython)
  (package! cython-mode :pin "8afd932c28d08428d45bba03d6b642093e4c973b")
  (when (modulep! :checkers syntax)
    (package! flycheck-cython :pin "ecc4454d35ab5317ab66a04406f36f0c1dbc0b76")))

;; LSP
(when (modulep! +lsp)
  (unless (modulep! :tools lsp +eglot)
    (if (modulep! +pyright)
        (package! lsp-pyright :pin "c745228f39fdb35372b29b909f25fa6c98dc7c88")
      (package! lsp-python-ms :pin "f8e7c4bcaefbc3fd96e1ca53d17589be0403b828"))))

;; Programming environment
(package! anaconda-mode :pin "160e4e7185881233d96da6722332bd6d038187a9")
(when (modulep! :completion company)
  (package! company-anaconda :pin "da1566db41a68809ef7f91ebf2de28118067c89b"))

;; Environment management
(package! pipenv :pin "3af159749824c03f59176aff7f66ddd6a5785a10")
(package! pyvenv :pin "31ea715f2164dd611e7fc77b26390ef3ca93509b")
(when (modulep! +pyenv)
  (package! pyenv-mode :pin "b818901b8eac0e260ced66a6a5acabdbf6f5ba99"))
(when (modulep! +conda)
  (package! conda :pin "a65ed0084876366fd1b238bb2b8b36ee75ac98d8"))
(when (modulep! +poetry)
  (package! poetry :pin "5b9ef569d629d79820e73b5380e54e443ba90616"))

;; Testing frameworks
(package! nose :pin "f8528297519eba911696c4e68fa88892de9a7b72")
(package! python-pytest :pin "9bf8db38bf18feb0484931877210cecfaa96bfc6")

;; Import managements
(package! pyimport :pin "a6f63cf7ed93f0c0f7c207e6595813966f8852b9")
(package! py-isort :pin "e67306f459c47c53a65604e4eea88a3914596560")
