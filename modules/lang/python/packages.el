;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; Major modes
(package! pip-requirements :pin "216cd1690f80cc965d4ae47b8753fc185f778ff6")
(when (featurep! +cython)
  (package! cython-mode :pin "0208bf2b71f478779491bf4a63a6b61de3d7269e")
  (when (featurep! :checkers syntax)
    (package! flycheck-cython :pin "ecc4454d35ab5317ab66a04406f36f0c1dbc0b76")))

;; LSP
(when (featurep! +lsp)
  (unless (featurep! :tools lsp +eglot)
    (if (featurep! +pyright)
        (package! lsp-pyright
          ;; REVIEW Remove this when added to melpa
          :recipe (:host github :repo "emacs-lsp/lsp-pyright")
          :pin "3cf2e8fc652e35270b1b39aeecc990d4a97de91f")
      (package! lsp-python-ms :pin "7a502e6c09456cbe8b5f6c64883c79f5ce08e5a9"))))

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
  (package! conda :pin "9d0213020ff170b17e11b35cad40ac9a8bf30f4c"))
(when (featurep! +poetry)
  (package! poetry :pin "22a76cdcba180b4689a6b45c97669e3c76cd36ed"))

;; Testing frameworks
(package! nose
  ;; REVIEW Remove this when emacsmirror/epkgs updates its emacsattic index
  :recipe (:host github :repo "emacsattic/nose")
  :pin "f8528297519eba911696c4e68fa88892de9a7b72")
(package! python-pytest :pin "6a3b4e560f26b5b8c9dca5699a3573f554592ac9")

;; Import managements
(package! pyimport :pin "a6f63cf7ed93f0c0f7c207e6595813966f8852b9")
(package! py-isort :pin "e67306f459c47c53a65604e4eea88a3914596560")
