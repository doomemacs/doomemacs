;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; requires: python jedi setuptools

(package! nose)
(package! pip-requirements)
(when (package! anaconda-mode)
  (when (featurep! :completion company)
    (package! company-anaconda)))
(when (featurep! +conda)
  (package! conda))
