;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

(package! anaconda-mode)
(package! nose)
(package! pip-requirements)

(when (featurep! :completion company)
  (package! company-anaconda))
