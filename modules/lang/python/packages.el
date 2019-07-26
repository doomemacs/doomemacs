;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; Major modes
(package! pip-requirements)

;; Microsoft LSP
(when (featurep! +lsp-ms)
  (package! lsp-python-ms))

;; Programming environment
(package! anaconda-mode)
(when (featurep! :completion company)
  (package! company-anaconda))

;; Environment management
(package! pipenv)
(package! pyvenv)
(when (featurep! +pyenv)
  (package! pyenv-mode))
(when (featurep! +conda)
  (package! conda))

;; Testing frameworks
(package! nose)
(package! python-pytest)

;; Import managements
(package! pyimport)
