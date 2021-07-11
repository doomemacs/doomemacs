;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; Major modes
(package! pip-requirements :pin "216cd1690f80cc965d4ae47b8753fc185f778ff6")
(when (featurep! +cython)
  (package! cython-mode :pin "fae33cf7d42559384deb7a9949f47b0881b0a29b")
  (when (featurep! :checkers syntax)
    (package! flycheck-cython :pin "ecc4454d35ab5317ab66a04406f36f0c1dbc0b76")))

;; LSP
(when (featurep! +lsp)
  (unless (featurep! :tools lsp +eglot)
    (if (featurep! +pyright)
        (package! lsp-pyright :pin "71a79760938d2132923fbff58dc25301892b1654")
      (package! lsp-python-ms :pin "4eb78c43046fceb53a66ccd24c85601bdb87ed17"))))

;; Programming environment
(package! anaconda-mode :pin "4f367c768a84465070c44327444b17015091d08d")
(when (featurep! :completion company)
  (package! company-anaconda :pin "da1566db41a68809ef7f91ebf2de28118067c89b"))

;; Environment management
(package! pipenv :pin "8f50c68d415307a2cbc65cc4df20df18e1776e9b")
(package! pyvenv :pin "045ff9476dac26086a04538d9b7ba186aa8f0fd1")
(when (featurep! +pyenv)
  (package! pyenv-mode :pin "b818901b8eac0e260ced66a6a5acabdbf6f5ba99"))
(when (featurep! +conda)
  (package! conda :pin "6db0720b6dc8880d7d6e7dc2953b4769ca6bbf71"))
(when (featurep! +poetry)
  (package! poetry :pin "d5163fe065239bb7b46ed8b3ff3b85b1f3229af3"))

;; Testing frameworks
(package! nose :pin "f8528297519eba911696c4e68fa88892de9a7b72")
(package! python-pytest :pin "31ae5e0e6813de8d889103f7b8dde252b04b1ae4")

;; Import managements
(package! pyimport :pin "a6f63cf7ed93f0c0f7c207e6595813966f8852b9")
(package! py-isort :pin "e67306f459c47c53a65604e4eea88a3914596560")
