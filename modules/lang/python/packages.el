;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; Major modes
(package! pip-requirements :pin "216cd1690f80cc965d4ae47b8753fc185f778ff6")
(when (featurep! +cython)
  (package! cython-mode :pin "5d8527d196c2ffd45b345ae31cac856f0c8fdc07")
  (when (featurep! :checkers syntax)
    (package! flycheck-cython :pin "ecc4454d35ab5317ab66a04406f36f0c1dbc0b76")))

;; LSP
(when (featurep! +lsp)
  (unless (featurep! :tools lsp +eglot)
    (if (featurep! +pyright)
        (package! lsp-pyright
          ;; REVIEW Remove this when added to melpa
          :recipe (:host github :repo "emacs-lsp/lsp-pyright")
          :pin "f85ad03477db5cd948a82fb7c6a75ac2bc5f0220")
      (package! lsp-python-ms :pin "d42ffc2cc27ce36b5a7646ea922441f4c93b2678"))))

;; Programming environment
(package! anaconda-mode :pin "6094dedf77810a47e213738f7e39a077de575ce1")
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
  (package! poetry :pin "d876522e5af576d53c62b2838f85c9441fe62258"))

;; Testing frameworks
(package! nose
  ;; REVIEW Remove this when emacsmirror/epkgs updates its emacsattic index
  :recipe (:host github :repo "emacsattic/nose")
  :pin "f8528297519eba911696c4e68fa88892de9a7b72")
(package! python-pytest :pin "dd059590d87569f2745452a69a7fa11f480864ee")

;; Import managements
(package! pyimport :pin "a6f63cf7ed93f0c0f7c207e6595813966f8852b9")
(package! py-isort :pin "e67306f459c47c53a65604e4eea88a3914596560")
