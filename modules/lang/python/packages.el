;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; requires: python jedi setuptools

(package! nose)
(package! pip-requirements)
(when (featurep! +conda)
  (package! conda))
(cond ((and (featurep! :tools +lsp)
            (featurep! +lsp))
       (package! lsp-python))
      ((package! anaconda-mode)
       (when (featurep! :completion company)
         (package! company-anaconda))))
