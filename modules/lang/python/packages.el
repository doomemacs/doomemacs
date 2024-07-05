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
      (package! lsp-pyright :pin "cc6df06aeaee3053eb800a24193483387b7b545b"))))

;; Programming environment
(package! anaconda-mode :pin "79fa9b4d2bda9f69857aeffb30c75276848a2810")
(when (modulep! :completion company)
  (package! company-anaconda :pin "1fe526163c265891cc20d971dc58b661ad8bcf23"))

;; Environment management
(package! pipenv :pin "3af159749824c03f59176aff7f66ddd6a5785a10")
(package! pyvenv :pin "31ea715f2164dd611e7fc77b26390ef3ca93509b")
(when (modulep! +pyenv)
  (package! pyenv-mode :pin "de0d750b9c3057fd7cecc72e6a290902a0475afe"))
(when (modulep! +conda)
  (package! conda :pin "60e14d1e9793431b91913a5688e278bd91d56224"))
(when (modulep! +poetry)
  (package! poetry :pin "1dff0d4a51ea8aff5f6ce97b154ea799902639ad"))

;; Testing frameworks
(package! nose :pin "f8528297519eba911696c4e68fa88892de9a7b72")
(package! python-pytest :pin "9f850e22df93812a5b109968c549f9b3dd828ed1")

;; Import managements
(package! pyimport :pin "4398ce8dd64fa0f685f4bf8683a35087649346d3")
(package! py-isort :pin "e67306f459c47c53a65604e4eea88a3914596560")
