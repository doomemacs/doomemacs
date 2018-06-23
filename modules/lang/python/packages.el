;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; requires: python jedi setuptools

(package! nose)
(package! pip-requirements)
(cond (when(featurep! :tools +lsp)
        (featurep! +lsp))
      (when (package! anaconda-mode)
        (when (featurep! :completion company)
          (package! company-anaconda))))
(when (featurep! +conda)
  (package! conda))
