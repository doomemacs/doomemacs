;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; requires: python setuptools

(package! nose)
(package! python-pytest)
(package! pip-requirements)

;; Environmet management
(package! pipenv)
(when (featurep! +pyenv)
  (package! pyenv-mode))
(when (featurep! +pyvenv)
  (package! pyvenv))
(when (featurep! +conda)
  (package! conda))

;; Programming environment
(when (package! anaconda-mode)
  (when (featurep! :completion company)
    (package! company-anaconda)))
