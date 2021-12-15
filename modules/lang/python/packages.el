;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; Major modes
(package! pip-requirements :pin "216cd1690f80cc965d4ae47b8753fc185f778ff6")
(when (featurep! +cython)
  (package! cython-mode :pin "5b325c98609a11e89c2d14f52afba25744a79379")
  (when (featurep! :checkers syntax)
    (package! flycheck-cython :pin "ecc4454d35ab5317ab66a04406f36f0c1dbc0b76")))

;; LSP
(when (featurep! +lsp)
  (unless (featurep! :tools lsp +eglot)
    (if (featurep! +pyright)
        (package! lsp-pyright :pin "d428dbcf1802fbe147271c8dc74b073bd9cd7403")
      (package! lsp-python-ms :pin "abf4d89ecf2fa0871130df5fce6065b7cf0a2721"))))

;; Programming environment
(package! anaconda-mode :pin "cbea0fb3182321d34ff93981c5a59f8dd72d82a5")
(when (featurep! :completion company)
  (package! company-anaconda :pin "da1566db41a68809ef7f91ebf2de28118067c89b"))

;; Environment management
(package! pipenv :pin "8f50c68d415307a2cbc65cc4df20df18e1776e9b")
(package! pyvenv :pin "31ea715f2164dd611e7fc77b26390ef3ca93509b")
(when (featurep! +pyenv)
  (package! pyenv-mode :pin "b818901b8eac0e260ced66a6a5acabdbf6f5ba99"))
(when (featurep! +conda)
  (package! conda :pin "7a34e06931515d46f9e22154762e06e66cfbc81c"))
(when (featurep! +poetry)
  (package! poetry :pin "5b9ef569d629d79820e73b5380e54e443ba90616"))

;; Testing frameworks
(package! nose :pin "f8528297519eba911696c4e68fa88892de9a7b72")
(package! python-pytest :pin "b603c5c7f21d351364deeb78e503d3a54d08a152")

;; Import managements
(package! pyimport :pin "a6f63cf7ed93f0c0f7c207e6595813966f8852b9")
(package! py-isort :pin "e67306f459c47c53a65604e4eea88a3914596560")
