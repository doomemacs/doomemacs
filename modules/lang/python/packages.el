;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; Major modes
(package! pip-requirements :pin "31e0dc62abb2d88fa765e0ea88b919d756cc0e4f")
(when (modulep! +cython)
  (package! cython-mode :pin "3e4790559d3168fe992cf2aa62f01423038cedb5")
  (when (modulep! :checkers syntax)
    (package! flycheck-cython :pin "ecc4454d35ab5317ab66a04406f36f0c1dbc0b76")))

;; LSP
(when (modulep! +lsp)
  (unless (modulep! :tools lsp +eglot)
    (when (modulep! +pyright)
      (package! lsp-pyright :pin "21b8f487855feb08f7df669b8884fbd5861dca25"))))

;; Environment management
(package! pipenv :pin "3af159749824c03f59176aff7f66ddd6a5785a10")
(package! pyvenv :pin "31ea715f2164dd611e7fc77b26390ef3ca93509b")
(when (modulep! +pyenv)
  (package! pyenv-mode :pin "8e5128ff7f722a4d68ddaa22022cb99ef9ddcf9a"))
(when (modulep! +uv)
  (package! uv-mode :pin "7e7f9b90832210b65823c3d58e3255cd164394b7"))
(when (modulep! +conda)
  (package! conda :pin "82b9f77a7f7d5c6ea91e06c5bd54d8a43a75f977"))
(when (modulep! +poetry)
  (package! poetry :pin "1dff0d4a51ea8aff5f6ce97b154ea799902639ad"))

;; Testing frameworks
(package! nose :pin "f8528297519eba911696c4e68fa88892de9a7b72")
(package! python-pytest :pin "78b5ea1d19c7e365ac00649d13c733954b11f822")

;; Import managements
(package! pyimport :pin "4398ce8dd64fa0f685f4bf8683a35087649346d3")
(package! py-isort :pin "e67306f459c47c53a65604e4eea88a3914596560")
