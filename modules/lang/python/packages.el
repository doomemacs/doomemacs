;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; requires: python setuptools

(package! nose)
(package! python-pytest)
(package! pip-requirements)
(when (featurep! +conda)
  (package! conda))
(cond ((and (featurep! :tools +lsp)
            (featurep! +lsp))
       (package! lsp-python))
      ((package! anaconda-mode)
       (when (featurep! :completion company)
         (package! company-anaconda))))

;; Environmet management
(package! pipenv)
(when (featurep! +pyenv)
  (package! pyenv-mode))
(when (featurep! +pyvenv)
  (package! pyvenv))
