;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; Major modes
(package! pip-requirements :pin "216cd1690f80cc965d4ae47b8753fc185f778ff6")
(when (featurep! +cython)
  (package! cython-mode :pin "9decfca1b7c836247c80ea94aca8c7cacf70327c")
  (when (featurep! :checkers syntax)
    (package! flycheck-cython :pin "ecc4454d35ab5317ab66a04406f36f0c1dbc0b76")))

;; LSP
(when (featurep! +lsp)
  (unless (featurep! :tools lsp +eglot)
    (if (featurep! +pyright)
        (package! lsp-pyright :pin "65fb14128127fb1ddf68dd4cb3140d6c7911a093")
      (package! lsp-python-ms :pin "689f6cf815c8ee2ca2332f31dfda8ddefb0b7e26"))))

;; Programming environment
(package! anaconda-mode :pin "344727c9e07e108896740c782689bf3588edcce5")
(when (featurep! :completion company)
  (package! company-anaconda :pin "da1566db41a68809ef7f91ebf2de28118067c89b"))

;; Environment management
(package! pipenv :pin "8f50c68d415307a2cbc65cc4df20df18e1776e9b")
(package! pyvenv :pin "9b3678bc29192d2dba64df90fbdb17393ef8d877")
(when (featurep! +pyenv)
  (package! pyenv-mode :pin "b818901b8eac0e260ced66a6a5acabdbf6f5ba99"))
(when (featurep! +conda)
  (package! conda :pin "dce431b25f5a13af58cc7cacfa7968b5a888609c"))
(when (featurep! +poetry)
  (package! poetry :pin "d5163fe065239bb7b46ed8b3ff3b85b1f3229af3"))

;; Testing frameworks
(package! nose :pin "f8528297519eba911696c4e68fa88892de9a7b72")
(package! python-pytest :pin "31ae5e0e6813de8d889103f7b8dde252b04b1ae4")

;; Import managements
(package! pyimport :pin "a6f63cf7ed93f0c0f7c207e6595813966f8852b9")
(package! py-isort :pin "e67306f459c47c53a65604e4eea88a3914596560")
